use serde::{Deserialize, Serialize};
use worker::*;

use turbo_m_core::engine;
use turbo_m_core::model::*;

// ── Schema ──────────────────────────────────────────────────────────────────

const SCHEMA_DECKS: &str = "CREATE TABLE IF NOT EXISTS decks (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, metadata TEXT NOT NULL DEFAULT '{}', fsrs_params TEXT NOT NULL DEFAULT '{}', created_at DATETIME DEFAULT CURRENT_TIMESTAMP)";

const SCHEMA_CARDS: &str = "CREATE TABLE IF NOT EXISTS cards (id INTEGER PRIMARY KEY AUTOINCREMENT, deck_id INTEGER NOT NULL REFERENCES decks(id) ON DELETE CASCADE, content TEXT NOT NULL, state INTEGER NOT NULL DEFAULT 0, due DATETIME, stability REAL NOT NULL DEFAULT 0.0, difficulty REAL NOT NULL DEFAULT 0.0, elapsed_days INTEGER NOT NULL DEFAULT 0, scheduled_days INTEGER NOT NULL DEFAULT 0, reps INTEGER NOT NULL DEFAULT 0, lapses INTEGER NOT NULL DEFAULT 0, last_review DATETIME, created_at DATETIME DEFAULT CURRENT_TIMESTAMP)";

const SCHEMA_REVLOG: &str = "CREATE TABLE IF NOT EXISTS revlog (id INTEGER PRIMARY KEY AUTOINCREMENT, card_id INTEGER NOT NULL REFERENCES cards(id) ON DELETE CASCADE, rating INTEGER NOT NULL, state INTEGER NOT NULL, due DATETIME NOT NULL, stability REAL NOT NULL, difficulty REAL NOT NULL, review_date DATETIME DEFAULT CURRENT_TIMESTAMP)";

// ── Request/Response types ──────────────────────────────────────────────────

#[derive(Deserialize)]
struct CreateDeckRequest {
    name: String,
    #[serde(default = "default_object")]
    metadata: serde_json::Value,
}

#[derive(Deserialize)]
struct UpdateDeckRequest {
    name: Option<String>,
    metadata: Option<serde_json::Value>,
    fsrs_params: Option<serde_json::Value>,
}

#[derive(Deserialize)]
struct AddCardRequest {
    content: Option<serde_json::Value>,
    contents: Option<Vec<serde_json::Value>>,
}

#[derive(Deserialize)]
struct ImportRequest {
    contents: Vec<serde_json::Value>,
}

#[derive(Serialize)]
struct IdResponse {
    id: i64,
}

#[derive(Serialize)]
struct CountResponse {
    count: u32,
}

fn default_object() -> serde_json::Value {
    serde_json::json!({})
}

// ── Auth ────────────────────────────────────────────────────────────────────

fn check_auth(req: &Request, env: &Env) -> Result<()> {
    let expected = env.var("API_TOKEN")?.to_string();
    let auth = req
        .headers()
        .get("Authorization")?
        .ok_or_else(|| Error::from("Missing Authorization header"))?;

    if !auth.starts_with("Bearer ") {
        return Err(Error::from("Invalid auth scheme"));
    }

    let token = &auth[7..];
    if token != expected {
        return Err(Error::from("Invalid token"));
    }

    Ok(())
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn get_db(env: &Env) -> Result<D1Database> {
    env.d1("DB")
}

async fn init_schema(db: &D1Database) -> Result<()> {
    let stmts = vec![
        db.prepare(SCHEMA_DECKS),
        db.prepare(SCHEMA_CARDS),
        db.prepare(SCHEMA_REVLOG),
    ];
    db.batch(stmts).await?;
    Ok(())
}

async fn deck_id_by_name(db: &D1Database, name: &str) -> Result<i64> {
    let stmt = query!(db, "SELECT id FROM decks WHERE name = ?1", name)?;
    let row = stmt
        .first::<serde_json::Value>(None)
        .await?
        .ok_or_else(|| Error::from(format!("Deck not found: {name}")))?;
    row["id"]
        .as_i64()
        .ok_or_else(|| Error::from("Invalid deck id"))
}

fn parse_limit_from_url(url: &Url) -> u32 {
    url.query_pairs()
        .find(|(k, _)| k == "limit")
        .and_then(|(_, v)| v.parse::<u32>().ok())
        .unwrap_or(12)
}

// ── Route Handlers ──────────────────────────────────────────────────────────

#[event(fetch, respond_with_errors)]
pub async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    Router::new()
        // Initialize schema
        .post_async("/api/init", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            init_schema(&db).await?;
            Response::ok("schema initialized")
        })
        // ── Deck routes ─────────────────────────────────────────────────
        .post_async("/api/decks", |mut req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let body: CreateDeckRequest = req.json().await?;
            let meta_str = serde_json::to_string(&body.metadata)
                .map_err(|e| Error::from(e.to_string()))?;

            let stmt = query!(
                &db,
                "INSERT INTO decks (name, metadata) VALUES (?1, ?2)",
                &body.name,
                &meta_str
            )?;
            let result = stmt.run().await?;
            let id = result
                .meta()
                .ok()
                .flatten()
                .and_then(|m| m.last_row_id)
                .unwrap_or(0);

            let deck = Deck {
                id,
                name: body.name,
                metadata: body.metadata,
                fsrs_params: serde_json::json!({}),
            };
            Response::from_json(&deck)
        })
        .get_async("/api/decks", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;

            let stmt = query!(&db, "SELECT id, name, metadata, fsrs_params FROM decks");
            let result = stmt.all().await?;

            #[derive(Deserialize)]
            struct DeckRow {
                id: i64,
                name: String,
                metadata: String,
                fsrs_params: String,
            }
            let rows = result.results::<DeckRow>()?;
            let decks: Vec<Deck> = rows
                .into_iter()
                .map(|r| Deck {
                    id: r.id,
                    name: r.name,
                    metadata: serde_json::from_str(&r.metadata).unwrap_or(serde_json::json!({})),
                    fsrs_params: serde_json::from_str(&r.fsrs_params)
                        .unwrap_or(serde_json::json!({})),
                })
                .collect();
            Response::from_json(&decks)
        })
        .put_async("/api/decks/:name", |mut req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("name").unwrap().clone();
            let body: UpdateDeckRequest = req.json().await?;

            let id = deck_id_by_name(&db, &deck_name).await?;

            if let Some(ref n) = body.name {
                query!(&db, "UPDATE decks SET name = ?1 WHERE id = ?2", n, &id)?
                    .run()
                    .await?;
            }
            if let Some(ref m) = body.metadata {
                let meta_str =
                    serde_json::to_string(m).map_err(|e| Error::from(e.to_string()))?;
                query!(
                    &db,
                    "UPDATE decks SET metadata = ?1 WHERE id = ?2",
                    &meta_str,
                    &id
                )?
                .run()
                .await?;
            }
            if let Some(ref p) = body.fsrs_params {
                let params_str =
                    serde_json::to_string(p).map_err(|e| Error::from(e.to_string()))?;
                query!(
                    &db,
                    "UPDATE decks SET fsrs_params = ?1 WHERE id = ?2",
                    &params_str,
                    &id
                )?
                .run()
                .await?;
            }

            // Re-fetch the deck
            #[derive(Deserialize)]
            struct DeckRow {
                id: i64,
                name: String,
                metadata: String,
                fsrs_params: String,
            }
            let stmt = query!(
                &db,
                "SELECT id, name, metadata, fsrs_params FROM decks WHERE id = ?1",
                &id
            )?;
            let row = stmt
                .first::<DeckRow>(None)
                .await?
                .ok_or_else(|| Error::from("Deck disappeared"))?;
            let deck = Deck {
                id: row.id,
                name: row.name,
                metadata: serde_json::from_str(&row.metadata).unwrap_or(serde_json::json!({})),
                fsrs_params: serde_json::from_str(&row.fsrs_params)
                    .unwrap_or(serde_json::json!({})),
            };
            Response::from_json(&deck)
        })
        // ── Card routes ─────────────────────────────────────────────────
        .post_async("/api/decks/:deck/cards", |mut req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let body: AddCardRequest = req.json().await?;

            if let Some(content) = body.content {
                // Single card
                let content_str =
                    serde_json::to_string(&content).map_err(|e| Error::from(e.to_string()))?;
                let stmt = query!(
                    &db,
                    "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
                    &deck_id,
                    &content_str
                )?;
                let result = stmt.run().await?;
                let id = result
                    .meta()
                    .ok()
                    .flatten()
                    .and_then(|m| m.last_row_id)
                    .unwrap_or(0);
                Response::from_json(&IdResponse { id })
            } else if let Some(contents) = body.contents {
                // Batch cards
                let mut ids = Vec::with_capacity(contents.len());
                for content in contents {
                    let content_str = serde_json::to_string(&content)
                        .map_err(|e| Error::from(e.to_string()))?;
                    let stmt = query!(
                        &db,
                        "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
                        &deck_id,
                        &content_str
                    )?;
                    let result = stmt.run().await?;
                    let id = result
                        .meta()
                        .ok()
                        .flatten()
                        .and_then(|m| m.last_row_id)
                        .unwrap_or(0);
                    ids.push(id);
                }
                Response::from_json(&ids)
            } else {
                Response::error("Must provide 'content' or 'contents'", 400)
            }
        })
        .post_async("/api/decks/:deck/import", |mut req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let body: ImportRequest = req.json().await?;
            let total = body.contents.len();

            // Fetch existing content strings
            let stmt = query!(
                &db,
                "SELECT content FROM cards WHERE deck_id = ?1",
                &deck_id
            )?;
            let result = stmt.all().await?;
            #[derive(Deserialize)]
            struct ContentRow {
                content: String,
            }
            let rows = result.results::<ContentRow>()?;
            let mut existing: std::collections::HashSet<String> =
                rows.into_iter().map(|r| r.content).collect();

            let mut imported = 0usize;
            for content in body.contents {
                let content_str =
                    serde_json::to_string(&content).map_err(|e| Error::from(e.to_string()))?;
                if existing.contains(&content_str) {
                    continue;
                }
                query!(
                    &db,
                    "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
                    &deck_id,
                    &content_str
                )?
                .run()
                .await?;
                existing.insert(content_str);
                imported += 1;
            }

            let result = ImportResult {
                imported,
                skipped: total - imported,
                total,
            };
            Response::from_json(&result)
        })
        .get_async("/api/decks/:deck/due", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let limit = parse_limit_from_url(&req.url()?);
            let now = chrono::Utc::now().to_rfc3339();

            let stmt = query!(
                &db,
                "SELECT id, deck_id, content, state, due FROM cards
                 WHERE deck_id = ?1 AND state != 0 AND due <= ?2
                 ORDER BY due ASC LIMIT ?3",
                &deck_id,
                &now,
                &limit
            )?;
            let result = stmt.all().await?;

            #[derive(Deserialize)]
            struct Row {
                id: i64,
                deck_id: i64,
                content: String,
                state: u8,
                due: Option<String>,
            }
            let rows = result.results::<Row>()?;
            let cards: Vec<CardDTO> = rows
                .into_iter()
                .filter_map(|r| {
                    Some(CardDTO {
                        id: r.id,
                        deck_id: r.deck_id,
                        content: serde_json::from_str(&r.content).ok()?,
                        due: r.due,
                        state: CardState::try_from(r.state).ok()?,
                    })
                })
                .collect();
            Response::from_json(&cards)
        })
        .get_async("/api/decks/:deck/count-due", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let now = chrono::Utc::now().to_rfc3339();

            let stmt = query!(
                &db,
                "SELECT COUNT(*) as count FROM cards WHERE deck_id = ?1 AND state != 0 AND due <= ?2",
                &deck_id,
                &now
            )?;
            #[derive(Deserialize)]
            struct CountRow {
                count: u32,
            }
            let row = stmt
                .first::<CountRow>(None)
                .await?
                .unwrap_or(CountRow { count: 0 });
            Response::from_json(&CountResponse { count: row.count })
        })
        .get_async("/api/decks/:deck/new", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let limit = parse_limit_from_url(&req.url()?);

            let stmt = query!(
                &db,
                "SELECT id, deck_id, content, state, due FROM cards
                 WHERE deck_id = ?1 AND state = 0
                 ORDER BY id ASC LIMIT ?2",
                &deck_id,
                &limit
            )?;
            let result = stmt.all().await?;

            #[derive(Deserialize)]
            struct Row {
                id: i64,
                deck_id: i64,
                content: String,
                state: u8,
                due: Option<String>,
            }
            let rows = result.results::<Row>()?;
            let cards: Vec<CardDTO> = rows
                .into_iter()
                .filter_map(|r| {
                    Some(CardDTO {
                        id: r.id,
                        deck_id: r.deck_id,
                        content: serde_json::from_str(&r.content).ok()?,
                        due: r.due,
                        state: CardState::try_from(r.state).ok()?,
                    })
                })
                .collect();
            Response::from_json(&cards)
        })
        .get_async("/api/decks/:deck/hardest", |req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let deck_name = ctx.param("deck").unwrap().clone();
            let deck_id = deck_id_by_name(&db, &deck_name).await?;
            let limit = parse_limit_from_url(&req.url()?);

            let stmt = query!(
                &db,
                "SELECT id, deck_id, content, difficulty, lapses, reps, state FROM cards
                 WHERE deck_id = ?1 AND state != 0
                 ORDER BY difficulty DESC, lapses DESC LIMIT ?2",
                &deck_id,
                &limit
            )?;
            let result = stmt.all().await?;

            #[derive(Deserialize)]
            struct Row {
                id: i64,
                deck_id: i64,
                content: String,
                difficulty: f64,
                lapses: i32,
                reps: i32,
                state: u8,
            }
            let rows = result.results::<Row>()?;
            let cards: Vec<HardestCardDTO> = rows
                .into_iter()
                .filter_map(|r| {
                    Some(HardestCardDTO {
                        id: r.id,
                        deck_id: r.deck_id,
                        content: serde_json::from_str(&r.content).ok()?,
                        difficulty: r.difficulty,
                        lapses: r.lapses,
                        reps: r.reps,
                        state: CardState::try_from(r.state).ok()?,
                    })
                })
                .collect();
            Response::from_json(&cards)
        })
        // ── Review routes ───────────────────────────────────────────────
        .post_async("/api/reviews", |mut req, ctx| async move {
            check_auth(&req, &ctx.env)?;
            let db = get_db(&ctx.env)?;
            let reviews: Vec<ReviewSubmission> = req.json().await?;
            let fsrs = engine::make_fsrs();
            let now = chrono::Utc::now();

            for review in &reviews {
                // Load card
                #[derive(Deserialize)]
                struct FullCard {
                    id: i64,
                    deck_id: i64,
                    content: String,
                    state: u8,
                    due: Option<String>,
                    stability: f64,
                    difficulty: f64,
                    elapsed_days: i64,
                    scheduled_days: i64,
                    reps: i32,
                    lapses: i32,
                    last_review: Option<String>,
                }

                let stmt = query!(
                    &db,
                    "SELECT id, deck_id, content, state, due, stability, difficulty,
                            elapsed_days, scheduled_days, reps, lapses, last_review
                     FROM cards WHERE id = ?1",
                    &review.card_id
                )?;
                let row = stmt
                    .first::<FullCard>(None)
                    .await?
                    .ok_or_else(|| Error::from(format!("Card not found: {}", review.card_id)))?;

                let card_state = CardState::try_from(row.state)
                    .map_err(Error::from)?;
                let state_before_u8 = row.state;

                let card_row = CardRow {
                    id: row.id,
                    deck_id: row.deck_id,
                    content: row.content,
                    state: card_state,
                    due: row.due,
                    stability: row.stability,
                    difficulty: row.difficulty,
                    elapsed_days: row.elapsed_days,
                    scheduled_days: row.scheduled_days,
                    reps: row.reps,
                    lapses: row.lapses,
                    last_review: row.last_review,
                };

                let fsrs_card = engine::card_row_to_fsrs(&card_row);
                let rating = engine::to_fsrs_rating(review.rating);
                let scheduling_info = fsrs.next(fsrs_card, now, rating);
                let new_card = scheduling_info.card;
                let new_state = u8::from(CardState::from(new_card.state));
                let new_due = new_card.due.to_rfc3339();
                let new_stability = new_card.stability;
                let new_difficulty = new_card.difficulty;
                let new_elapsed = new_card.elapsed_days;
                let new_scheduled = new_card.scheduled_days;
                let new_reps = new_card.reps;
                let new_lapses = new_card.lapses;
                let now_str = now.to_rfc3339();
                let card_id = review.card_id;
                let rating_u8 = u8::from(review.rating);

                query!(
                    &db,
                    "UPDATE cards SET
                        state = ?1, due = ?2, stability = ?3, difficulty = ?4,
                        elapsed_days = ?5, scheduled_days = ?6, reps = ?7, lapses = ?8,
                        last_review = ?9
                     WHERE id = ?10",
                    &new_state,
                    &new_due,
                    &new_stability,
                    &new_difficulty,
                    &new_elapsed,
                    &new_scheduled,
                    &new_reps,
                    &new_lapses,
                    &now_str,
                    &card_id
                )?
                .run()
                .await?;

                query!(
                    &db,
                    "INSERT INTO revlog (card_id, rating, state, due, stability, difficulty)
                     VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                    &card_id,
                    &rating_u8,
                    &state_before_u8,
                    &new_due,
                    &new_stability,
                    &new_difficulty
                )?
                .run()
                .await?;
            }

            Response::from_json(&serde_json::json!({
                "status": "ok",
                "processed": reviews.len()
            }))
        })
        .run(req, env)
        .await
}
