use chrono::Utc;
use rusqlite::{Connection, params};
use serde_json::Value;

use crate::engine;
use crate::error::AppError;
use crate::model::{CardDTO, CardRow, Deck, ImportResult, ReviewSubmission};

/// Look up a deck ID by name, returning DeckNotFound if missing.
fn deck_id_by_name(conn: &Connection, name: &str) -> Result<i64, AppError> {
    conn.query_row(
        "SELECT id FROM decks WHERE name = ?1",
        params![name],
        |row| row.get(0),
    )
    .map_err(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => AppError::DeckNotFound(name.to_string()),
        other => AppError::Db(other),
    })
}

// ── Deck operations ──────────────────────────────────────────────────────

pub fn create_deck(conn: &Connection, name: &str, metadata: Value) -> Result<Deck, AppError> {
    let meta_str = serde_json::to_string(&metadata)?;
    conn.execute(
        "INSERT INTO decks (name, metadata) VALUES (?1, ?2)",
        params![name, meta_str],
    )?;
    let id = conn.last_insert_rowid();
    Ok(Deck {
        id,
        name: name.to_string(),
        metadata,
        fsrs_params: serde_json::json!({}),
    })
}

pub fn list_decks(conn: &Connection) -> Result<Vec<Deck>, AppError> {
    let mut stmt = conn.prepare("SELECT id, name, metadata, fsrs_params FROM decks")?;
    let decks = stmt
        .query_map([], |row| {
            let meta_str: String = row.get(2)?;
            let params_str: String = row.get(3)?;
            Ok(Deck {
                id: row.get(0)?,
                name: row.get(1)?,
                metadata: serde_json::from_str(&meta_str).unwrap_or(serde_json::json!({})),
                fsrs_params: serde_json::from_str(&params_str).unwrap_or(serde_json::json!({})),
            })
        })?
        .collect::<Result<Vec<_>, _>>()?;
    Ok(decks)
}

pub fn update_deck(
    conn: &Connection,
    name: &str,
    new_name: Option<&str>,
    metadata: Option<Value>,
    fsrs_params: Option<Value>,
) -> Result<Deck, AppError> {
    let tx = conn.unchecked_transaction()?;

    let id = deck_id_by_name(&tx, name)?;

    if let Some(n) = new_name {
        tx.execute("UPDATE decks SET name = ?1 WHERE id = ?2", params![n, id])?;
    }
    if let Some(ref m) = metadata {
        let meta_str = serde_json::to_string(m)?;
        tx.execute(
            "UPDATE decks SET metadata = ?1 WHERE id = ?2",
            params![meta_str, id],
        )?;
    }
    if let Some(ref p) = fsrs_params {
        let params_str = serde_json::to_string(p)?;
        tx.execute(
            "UPDATE decks SET fsrs_params = ?1 WHERE id = ?2",
            params![params_str, id],
        )?;
    }

    let final_name = new_name.unwrap_or(name);
    let deck = tx
        .query_row(
            "SELECT id, name, metadata, fsrs_params FROM decks WHERE id = ?1",
            params![id],
            |row| {
                let meta_str: String = row.get(2)?;
                let params_str: String = row.get(3)?;
                Ok(Deck {
                    id: row.get(0)?,
                    name: row.get(1)?,
                    metadata: serde_json::from_str(&meta_str).unwrap_or(serde_json::json!({})),
                    fsrs_params: serde_json::from_str(&params_str).unwrap_or(serde_json::json!({})),
                })
            },
        )
        .map_err(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => AppError::DeckNotFound(final_name.to_string()),
        other => AppError::Db(other),
    })?;

    tx.commit()?;
    Ok(deck)
}

// ── Card operations ──────────────────────────────────────────────────────

pub fn add_card(conn: &Connection, deck_name: &str, content: Value) -> Result<i64, AppError> {
    let deck_id = deck_id_by_name(conn, deck_name)?;
    let content_str = serde_json::to_string(&content)?;
    conn.execute(
        "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
        params![deck_id, content_str],
    )?;
    Ok(conn.last_insert_rowid())
}

pub fn add_cards(
    conn: &Connection,
    deck_name: &str,
    contents: Vec<Value>,
) -> Result<Vec<i64>, AppError> {
    let deck_id = deck_id_by_name(conn, deck_name)?;
    let tx = conn.unchecked_transaction()?;
    let mut ids = Vec::with_capacity(contents.len());

    for content in contents {
        let content_str = serde_json::to_string(&content)?;
        tx.execute(
            "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
            params![deck_id, content_str],
        )?;
        ids.push(tx.last_insert_rowid());
    }

    tx.commit()?;
    Ok(ids)
}

/// Import cards into a deck, skipping any whose content already exists in that deck.
pub fn import_cards(
    conn: &Connection,
    deck_name: &str,
    contents: Vec<Value>,
) -> Result<ImportResult, AppError> {
    let deck_id = deck_id_by_name(conn, deck_name)?;
    let total = contents.len();
    let tx = conn.unchecked_transaction()?;

    // Collect all existing content strings for this deck into a set
    let mut existing: std::collections::HashSet<String> = std::collections::HashSet::new();
    {
        let mut stmt = tx.prepare("SELECT content FROM cards WHERE deck_id = ?1")?;
        let rows = stmt.query_map(params![deck_id], |row| row.get::<_, String>(0))?;
        for row in rows {
            existing.insert(row?);
        }
    }

    let mut imported = 0;
    for content in contents {
        let content_str = serde_json::to_string(&content)?;
        if existing.contains(&content_str) {
            continue;
        }
        tx.execute(
            "INSERT INTO cards (deck_id, content) VALUES (?1, ?2)",
            params![deck_id, content_str],
        )?;
        existing.insert(content_str);
        imported += 1;
    }

    tx.commit()?;
    Ok(ImportResult {
        imported,
        skipped: total - imported,
        total,
    })
}

/// Fetch cards that are due for review (state != New and due <= now).
pub fn fetch_due(conn: &Connection, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
    let deck_id = deck_id_by_name(conn, deck_name)?;
    let now = Utc::now().to_rfc3339();
    let mut stmt = conn.prepare(
        "SELECT id, deck_id, content, state, due
         FROM cards
         WHERE deck_id = ?1 AND state != 0 AND due <= ?3
         ORDER BY due ASC
         LIMIT ?2",
    )?;

    let cards = stmt
        .query_map(params![deck_id, limit, now], |row| {
            let content_str: String = row.get(2)?;
            Ok(CardDTO {
                id: row.get(0)?,
                deck_id: row.get(1)?,
                content: serde_json::from_str(&content_str).unwrap_or(Value::Null),
                due: row.get(4)?,
                state: row.get(3)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()?;

    Ok(cards)
}

/// Fetch new (unreviewed) cards for a warm-up session.
pub fn fetch_new(conn: &Connection, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
    let deck_id = deck_id_by_name(conn, deck_name)?;
    let mut stmt = conn.prepare(
        "SELECT id, deck_id, content, state, due
         FROM cards
         WHERE deck_id = ?1 AND state = 0
         ORDER BY id ASC
         LIMIT ?2",
    )?;

    let cards = stmt
        .query_map(params![deck_id, limit], |row| {
            let content_str: String = row.get(2)?;
            Ok(CardDTO {
                id: row.get(0)?,
                deck_id: row.get(1)?,
                content: serde_json::from_str(&content_str).unwrap_or(Value::Null),
                due: row.get(4)?,
                state: row.get(3)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()?;

    Ok(cards)
}

/// Load a full card row from the database for FSRS processing.
fn get_card_row(conn: &Connection, card_id: i64) -> Result<CardRow, AppError> {
    conn.query_row(
        "SELECT id, deck_id, content, state, due, stability, difficulty,
                elapsed_days, scheduled_days, reps, lapses, last_review
         FROM cards WHERE id = ?1",
        params![card_id],
        |row| {
            Ok(CardRow {
                id: row.get(0)?,
                deck_id: row.get(1)?,
                content: row.get(2)?,
                state: row.get(3)?,
                due: row.get(4)?,
                stability: row.get(5)?,
                difficulty: row.get(6)?,
                elapsed_days: row.get(7)?,
                scheduled_days: row.get(8)?,
                reps: row.get(9)?,
                lapses: row.get(10)?,
                last_review: row.get(11)?,
            })
        },
    )
    .map_err(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => AppError::CardNotFound(card_id),
        other => AppError::Db(other),
    })
}

/// Process a batch of review submissions: update FSRS state and write to revlog.
/// The entire batch is wrapped in a single transaction.
pub fn process_reviews(conn: &Connection, reviews: &[ReviewSubmission]) -> Result<(), AppError> {
    let tx = conn.unchecked_transaction()?;
    let fsrs = engine::make_fsrs();
    let now = Utc::now();

    for review in reviews {
        // 1. Load the current card state from DB
        let card_row = get_card_row(&tx, review.card_id)?;
        let state_before = card_row.state;

        // 2. Convert to rs-fsrs Card
        let fsrs_card = engine::card_row_to_fsrs(&card_row);

        // 3. Convert rating
        let rating = engine::rating_from_u32(review.rating)?;

        // 4. Run the FSRS scheduler
        let scheduling_info = fsrs.next(fsrs_card, now, rating);
        let new_card = scheduling_info.card;

        // 5. Update the card in the database
        tx.execute(
            "UPDATE cards SET
                state = ?1, due = ?2, stability = ?3, difficulty = ?4,
                elapsed_days = ?5, scheduled_days = ?6, reps = ?7, lapses = ?8,
                last_review = ?9
             WHERE id = ?10",
            params![
                engine::state_to_u8(new_card.state),
                new_card.due.to_rfc3339(),
                new_card.stability,
                new_card.difficulty,
                new_card.elapsed_days as i64,
                new_card.scheduled_days as i64,
                new_card.reps as i32,
                new_card.lapses as i32,
                now.to_rfc3339(),
                review.card_id,
            ],
        )?;

        // 6. Insert into the review log
        tx.execute(
            "INSERT INTO revlog (card_id, rating, state, due, stability, difficulty)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                review.card_id,
                review.rating,
                state_before,
                new_card.due.to_rfc3339(),
                new_card.stability,
                new_card.difficulty,
            ],
        )?;
    }

    tx.commit()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::db;

    /// Helper: create an in-memory DB with schema initialized.
    fn test_conn() -> Connection {
        let conn = Connection::open_in_memory().unwrap();
        db::init_db(&conn).unwrap();
        conn
    }

    /// Helper: create a deck and add a card, returning (deck_name, card_id).
    fn seed_one_card(conn: &Connection) -> (String, i64) {
        let deck_name = "test-deck";
        create_deck(conn, deck_name, serde_json::json!({})).unwrap();
        let card_id =
            add_card(conn, deck_name, serde_json::json!({"front":"a","back":"b"})).unwrap();
        (deck_name.to_string(), card_id)
    }

    /// Simulate what process_reviews does: write a card's due date in RFC 3339
    /// format and set state to non-zero (Learning=1), then confirm fetch_due
    /// can find it.
    ///
    /// This test demonstrates the bug: due dates are stored as RFC 3339
    /// (e.g. "2024-01-15T10:30:00+00:00") but fetch_due compared against
    /// datetime('now') which returns "2024-01-15 10:30:00". Since 'T' > ' '
    /// in ASCII, the comparison `due <= datetime('now')` was always false.
    #[test]
    fn fetch_due_finds_cards_with_rfc3339_due_dates() {
        let conn = test_conn();
        let (deck_name, card_id) = seed_one_card(&conn);

        // Set the card to Learning state with a due date in the past, using
        // the same RFC 3339 format that process_reviews writes.
        let past_due = chrono::Utc::now() - chrono::Duration::hours(1);
        conn.execute(
            "UPDATE cards SET state = 1, due = ?1 WHERE id = ?2",
            params![past_due.to_rfc3339(), card_id],
        )
        .unwrap();

        let due_cards = fetch_due(&conn, &deck_name, 10).unwrap();
        assert_eq!(
            due_cards.len(),
            1,
            "fetch_due must find cards whose due date (RFC 3339) is in the past"
        );
        assert_eq!(due_cards[0].id, card_id);
    }

    /// A card whose due date is in the future should NOT be returned.
    #[test]
    fn fetch_due_excludes_future_cards() {
        let conn = test_conn();
        let (deck_name, card_id) = seed_one_card(&conn);

        let future_due = chrono::Utc::now() + chrono::Duration::hours(1);
        conn.execute(
            "UPDATE cards SET state = 1, due = ?1 WHERE id = ?2",
            params![future_due.to_rfc3339(), card_id],
        )
        .unwrap();

        let due_cards = fetch_due(&conn, &deck_name, 10).unwrap();
        assert_eq!(
            due_cards.len(),
            0,
            "fetch_due must NOT return cards whose due date is in the future"
        );
    }

    // ── update_deck atomicity tests ────────────────────────────────────

    /// A successful update_deck with all three fields should apply all changes.
    #[test]
    fn update_deck_applies_all_fields() {
        let conn = test_conn();
        create_deck(&conn, "d1", serde_json::json!({"k": "old"})).unwrap();

        let updated = update_deck(
            &conn,
            "d1",
            Some("d1-renamed"),
            Some(serde_json::json!({"k": "new"})),
            Some(serde_json::json!({"w": [1,2,3]})),
        )
        .unwrap();

        assert_eq!(updated.name, "d1-renamed");
        assert_eq!(updated.metadata, serde_json::json!({"k": "new"}));
        assert_eq!(updated.fsrs_params, serde_json::json!({"w": [1,2,3]}));
    }

    /// When update_deck renames to a name that already exists (UNIQUE
    /// violation), the entire operation must be atomic — metadata and
    /// fsrs_params must NOT be changed either.
    #[test]
    fn update_deck_rolls_back_on_rename_conflict() {
        let conn = test_conn();
        create_deck(&conn, "deck-a", serde_json::json!({"v": 1})).unwrap();
        create_deck(&conn, "deck-b", serde_json::json!({})).unwrap();

        // Try to rename deck-a → deck-b (conflict) while also changing metadata.
        let result = update_deck(
            &conn,
            "deck-a",
            Some("deck-b"),                     // will conflict with existing deck-b
            Some(serde_json::json!({"v": 2})),  // must NOT be applied
            Some(serde_json::json!({"p": 99})), // must NOT be applied
        );
        assert!(result.is_err(), "rename to existing name should fail");

        // Verify deck-a is completely untouched — name, metadata, and fsrs_params.
        let deck_a = conn
            .query_row(
                "SELECT name, metadata, fsrs_params FROM decks WHERE name = 'deck-a'",
                [],
                |row| {
                    Ok((
                        row.get::<_, String>(0)?,
                        row.get::<_, String>(1)?,
                        row.get::<_, String>(2)?,
                    ))
                },
            )
            .expect("deck-a should still exist under its original name");

        assert_eq!(deck_a.0, "deck-a");
        let meta: serde_json::Value = serde_json::from_str(&deck_a.1).unwrap();
        assert_eq!(
            meta,
            serde_json::json!({"v": 1}),
            "metadata must not have changed"
        );
        let params: serde_json::Value = serde_json::from_str(&deck_a.2).unwrap();
        assert_eq!(
            params,
            serde_json::json!({}),
            "fsrs_params must not have changed"
        );
    }

    /// Prove that update_deck must be atomic: if the rename succeeds but a
    /// later step fails, the rename must be rolled back.
    ///
    /// We install a SQLite TRIGGER that rejects metadata containing the
    /// marker "__reject__". This forces the metadata UPDATE to fail AFTER
    /// the rename has already executed. Without a transaction, the rename
    /// is auto-committed and the deck ends up with a new name but old
    /// metadata — a partial update.
    #[test]
    fn update_deck_rename_rolled_back_when_metadata_update_fails() {
        let conn = test_conn();
        create_deck(&conn, "orig", serde_json::json!({"v": 1})).unwrap();

        // Install a trigger that rejects metadata containing "__reject__".
        conn.execute_batch(
            "CREATE TRIGGER reject_metadata
             BEFORE UPDATE OF metadata ON decks
             WHEN NEW.metadata LIKE '%__reject__%'
             BEGIN
                 SELECT RAISE(ABORT, 'forced metadata failure');
             END;",
        )
        .unwrap();

        // Rename succeeds (no trigger on name), but metadata hits the trigger.
        let result = update_deck(
            &conn,
            "orig",
            Some("renamed"),
            Some(serde_json::json!({"__reject__": true})),
            None,
        );
        assert!(result.is_err(), "metadata trigger should cause an error");

        // The deck must still have its ORIGINAL name. If the rename was
        // auto-committed without a transaction, it would be "renamed".
        let original_exists: bool = conn
            .query_row(
                "SELECT COUNT(*) > 0 FROM decks WHERE name = 'orig'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert!(
            original_exists,
            "deck must still be named 'orig' — rename should have been rolled back"
        );
    }

    // ── Error handling tests ─────────────────────────────────────────

    #[test]
    fn deck_id_by_name_returns_deck_not_found_for_missing_deck() {
        let conn = test_conn();
        let err = deck_id_by_name(&conn, "nonexistent").unwrap_err();
        assert!(
            matches!(err, AppError::DeckNotFound(ref name) if name == "nonexistent"),
            "expected DeckNotFound, got: {err:?}"
        );
    }

    #[test]
    fn deck_id_by_name_returns_db_error_on_schema_issue() {
        let conn = Connection::open_in_memory().unwrap();
        let err = deck_id_by_name(&conn, "anything").unwrap_err();
        assert!(
            matches!(err, AppError::Db(_)),
            "expected Db error when table doesn't exist, got: {err:?}"
        );
    }

    #[test]
    fn get_card_row_returns_card_not_found_for_missing_card() {
        let conn = test_conn();
        let err = get_card_row(&conn, 9999).unwrap_err();
        assert!(
            matches!(err, AppError::CardNotFound(9999)),
            "expected CardNotFound(9999), got: {err:?}"
        );
    }

    #[test]
    fn get_card_row_returns_db_error_on_schema_issue() {
        let conn = Connection::open_in_memory().unwrap();
        let err = get_card_row(&conn, 1).unwrap_err();
        assert!(
            matches!(err, AppError::Db(_)),
            "expected Db error when table doesn't exist, got: {err:?}"
        );
    }

    #[test]
    fn add_card_to_nonexistent_deck_returns_deck_not_found() {
        let conn = test_conn();
        let err = add_card(&conn, "no-such-deck", serde_json::json!({"x": 1})).unwrap_err();
        assert!(
            matches!(err, AppError::DeckNotFound(ref name) if name == "no-such-deck"),
            "expected DeckNotFound, got: {err:?}"
        );
    }

    #[test]
    fn fetch_due_nonexistent_deck_returns_deck_not_found() {
        let conn = test_conn();
        let err = fetch_due(&conn, "ghost", 10).unwrap_err();
        assert!(
            matches!(err, AppError::DeckNotFound(ref name) if name == "ghost"),
            "expected DeckNotFound, got: {err:?}"
        );
    }

    #[test]
    fn process_reviews_missing_card_returns_card_not_found() {
        let conn = test_conn();
        let reviews = vec![ReviewSubmission {
            card_id: 42,
            rating: 3,
        }];
        let err = process_reviews(&conn, &reviews).unwrap_err();
        assert!(
            matches!(err, AppError::CardNotFound(42)),
            "expected CardNotFound(42), got: {err:?}"
        );
    }

    #[test]
    fn update_deck_nonexistent_returns_deck_not_found() {
        let conn = test_conn();
        let err = update_deck(&conn, "nope", Some("new-name"), None, None).unwrap_err();
        assert!(
            matches!(err, AppError::DeckNotFound(ref name) if name == "nope"),
            "expected DeckNotFound, got: {err:?}"
        );
    }

    /// End-to-end: add card → review it → the FSRS engine sets a due date →
    /// fetch_due must be able to find it once that date passes.
    #[test]
    fn fetch_due_after_process_reviews_round_trip() {
        let conn = test_conn();
        let (deck_name, card_id) = seed_one_card(&conn);

        // First review transitions New → Learning and sets a due date.
        let reviews = vec![ReviewSubmission {
            card_id,
            rating: 1, // Again — due date will be very soon (minutes)
        }];
        process_reviews(&conn, &reviews).unwrap();

        // Verify the card is no longer state=0 (New).
        let state: u8 = conn
            .query_row(
                "SELECT state FROM cards WHERE id = ?1",
                params![card_id],
                |r| r.get(0),
            )
            .unwrap();
        assert_ne!(state, 0, "card should have transitioned out of New state");

        // Read back the due date that FSRS wrote.
        let due_str: String = conn
            .query_row(
                "SELECT due FROM cards WHERE id = ?1",
                params![card_id],
                |r| r.get(0),
            )
            .unwrap();

        // Force the due date into the past so fetch_due should find it.
        let past = chrono::Utc::now() - chrono::Duration::hours(1);
        conn.execute(
            "UPDATE cards SET due = ?1 WHERE id = ?2",
            params![past.to_rfc3339(), card_id],
        )
        .unwrap();

        let due_cards = fetch_due(&conn, &deck_name, 10).unwrap();
        assert_eq!(
            due_cards.len(),
            1,
            "fetch_due must find a reviewed card with a past RFC 3339 due date; \
             stored due was: {due_str}"
        );
    }
}
