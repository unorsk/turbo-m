use std::collections::BTreeMap;

use chrono::{Duration, Utc};
use rusqlite::{Connection, params};

// ── ANSI colors ──────────────────────────────────────────────────────────

const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const RED: &str = "\x1b[31m";
const CYAN: &str = "\x1b[36m";
const MAGENTA: &str = "\x1b[35m";
const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";
const RESET: &str = "\x1b[0m";

const BAR_FULL: &str = "█";
const BAR_SEVEN: &str = "▉";
const BAR_SIX: &str = "▊";
const BAR_FIVE: &str = "▋";
const BAR_FOUR: &str = "▌";
const BAR_THREE: &str = "▍";
const BAR_TWO: &str = "▎";
const BAR_ONE: &str = "▏";

const BAR_CHARS: [&str; 8] = [
    BAR_ONE, BAR_TWO, BAR_THREE, BAR_FOUR, BAR_FIVE, BAR_SIX, BAR_SEVEN, BAR_FULL,
];

// ── Bar chart rendering ─────────────────────────────────────────────────

fn render_bar(value: usize, max_value: usize, max_width: usize, color: &str) -> String {
    if max_value == 0 {
        return String::new();
    }
    let ratio = value as f64 / max_value as f64;
    let full_width = ratio * max_width as f64;
    let full_blocks = full_width as usize;
    let remainder = ((full_width - full_blocks as f64) * 8.0) as usize;

    let mut bar = String::new();
    bar.push_str(color);
    for _ in 0..full_blocks {
        bar.push_str(BAR_FULL);
    }
    if remainder > 0 && full_blocks < max_width {
        bar.push_str(BAR_CHARS[remainder - 1]);
    }
    bar.push_str(RESET);
    bar
}

// ── Data types ──────────────────────────────────────────────────────────

struct DeckStats {
    name: String,
    total: usize,
    new: usize,
    learning: usize,
    review: usize,
    relearning: usize,
    due_today: usize,
}

struct ForecastBucket {
    label: String,
    count: usize,
}

struct ReviewDay {
    date: String,
    count: usize,
}

// ── Queries ─────────────────────────────────────────────────────────────

fn deck_overview(conn: &Connection, deck_filter: Option<&str>) -> Vec<DeckStats> {
    let today = Utc::now().format("%Y-%m-%dT%H:%M:%S").to_string();

    let deck_clause = if deck_filter.is_some() {
        "WHERE d.name = ?1"
    } else {
        ""
    };

    let sql = format!(
        "SELECT d.name,
                COUNT(c.id) as total,
                SUM(CASE WHEN c.state = 0 THEN 1 ELSE 0 END) as new,
                SUM(CASE WHEN c.state = 1 THEN 1 ELSE 0 END) as learning,
                SUM(CASE WHEN c.state = 2 THEN 1 ELSE 0 END) as review,
                SUM(CASE WHEN c.state = 3 THEN 1 ELSE 0 END) as relearning,
                SUM(CASE WHEN c.state != 0 AND c.due <= ?{param} THEN 1 ELSE 0 END) as due_today
         FROM decks d
         LEFT JOIN cards c ON c.deck_id = d.id
         {clause}
         GROUP BY d.id
         ORDER BY d.name",
        param = if deck_filter.is_some() { "2" } else { "1" },
        clause = deck_clause,
    );

    let mut stmt = conn.prepare(&sql).unwrap();
    let rows: Vec<DeckStats> = if let Some(name) = deck_filter {
        stmt.query_map(params![name, today], |row| {
            Ok(DeckStats {
                name: row.get(0)?,
                total: row.get::<_, i64>(1)? as usize,
                new: row.get::<_, i64>(2)? as usize,
                learning: row.get::<_, i64>(3)? as usize,
                review: row.get::<_, i64>(4)? as usize,
                relearning: row.get::<_, i64>(5)? as usize,
                due_today: row.get::<_, i64>(6)? as usize,
            })
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    } else {
        stmt.query_map(params![today], |row| {
            Ok(DeckStats {
                name: row.get(0)?,
                total: row.get::<_, i64>(1)? as usize,
                new: row.get::<_, i64>(2)? as usize,
                learning: row.get::<_, i64>(3)? as usize,
                review: row.get::<_, i64>(4)? as usize,
                relearning: row.get::<_, i64>(5)? as usize,
                due_today: row.get::<_, i64>(6)? as usize,
            })
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    };
    rows
}

fn due_forecast(conn: &Connection, deck_filter: Option<&str>) -> Vec<ForecastBucket> {
    let now = Utc::now();
    let boundaries: Vec<(&str, chrono::DateTime<Utc>, chrono::DateTime<Utc>)> = vec![
        ("Overdue", now - Duration::days(365 * 10), now),
        ("Today", now, now + Duration::days(1)),
        ("Tomorrow", now + Duration::days(1), now + Duration::days(2)),
        (
            "This week",
            now + Duration::days(2),
            now + Duration::days(7),
        ),
        (
            "Next week",
            now + Duration::days(7),
            now + Duration::days(14),
        ),
        (
            "This month",
            now + Duration::days(14),
            now + Duration::days(30),
        ),
        (
            "Later",
            now + Duration::days(30),
            now + Duration::days(365 * 10),
        ),
    ];

    let deck_clause = if deck_filter.is_some() {
        "AND c.deck_id = (SELECT id FROM decks WHERE name = ?3)"
    } else {
        ""
    };

    let mut buckets = Vec::new();
    for (label, start, end) in &boundaries {
        let sql = format!(
            "SELECT COUNT(*) FROM cards c
             WHERE c.state != 0 AND c.due IS NOT NULL
               AND c.due >= ?1 AND c.due < ?2
               {clause}",
            clause = deck_clause,
        );
        let mut stmt = conn.prepare(&sql).unwrap();
        let count: i64 = if let Some(name) = deck_filter {
            stmt.query_row(params![start.to_rfc3339(), end.to_rfc3339(), name], |row| {
                row.get(0)
            })
            .unwrap_or(0)
        } else {
            stmt.query_row(params![start.to_rfc3339(), end.to_rfc3339()], |row| {
                row.get(0)
            })
            .unwrap_or(0)
        };
        buckets.push(ForecastBucket {
            label: label.to_string(),
            count: count as usize,
        });
    }
    buckets
}

fn review_activity(conn: &Connection, deck_filter: Option<&str>, days: i64) -> Vec<ReviewDay> {
    let since = (Utc::now() - Duration::days(days))
        .format("%Y-%m-%d")
        .to_string();

    let deck_clause = if deck_filter.is_some() {
        "AND r.card_id IN (SELECT id FROM cards WHERE deck_id = (SELECT id FROM decks WHERE name = ?2))"
    } else {
        ""
    };

    let sql = format!(
        "SELECT date(r.review_date) as d, COUNT(*) as cnt
         FROM revlog r
         WHERE date(r.review_date) >= ?1
           {clause}
         GROUP BY d
         ORDER BY d ASC",
        clause = deck_clause,
    );

    let mut stmt = conn.prepare(&sql).unwrap();
    let rows: Vec<ReviewDay> = if let Some(name) = deck_filter {
        stmt.query_map(params![since, name], |row| {
            Ok(ReviewDay {
                date: row.get(0)?,
                count: row.get::<_, i64>(1)? as usize,
            })
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    } else {
        stmt.query_map(params![since], |row| {
            Ok(ReviewDay {
                date: row.get(0)?,
                count: row.get::<_, i64>(1)? as usize,
            })
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    };

    let mut by_date: BTreeMap<String, usize> = BTreeMap::new();
    for r in &rows {
        by_date.insert(r.date.clone(), r.count);
    }

    let today = Utc::now().date_naive();
    let start = today - Duration::days(days);
    let mut filled = Vec::new();
    let mut d = start;
    while d <= today {
        let key = d.format("%Y-%m-%d").to_string();
        let count = by_date.get(&key).copied().unwrap_or(0);
        filled.push(ReviewDay { date: key, count });
        d += Duration::days(1);
    }
    filled
}

fn rating_distribution(conn: &Connection, deck_filter: Option<&str>) -> [usize; 4] {
    let deck_clause = if deck_filter.is_some() {
        "WHERE r.card_id IN (SELECT id FROM cards WHERE deck_id = (SELECT id FROM decks WHERE name = ?1))"
    } else {
        ""
    };

    let sql = format!(
        "SELECT r.rating, COUNT(*) FROM revlog r {clause} GROUP BY r.rating",
        clause = deck_clause,
    );

    let mut dist = [0usize; 4];
    let mut stmt = conn.prepare(&sql).unwrap();
    let rows: Vec<(i64, i64)> = if let Some(name) = deck_filter {
        stmt.query_map(params![name], |row| Ok((row.get(0)?, row.get(1)?)))
            .unwrap()
            .filter_map(|r| r.ok())
            .collect()
    } else {
        stmt.query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
            .unwrap()
            .filter_map(|r| r.ok())
            .collect()
    };
    for (rating, count) in rows {
        if (1..=4).contains(&rating) {
            dist[(rating - 1) as usize] = count as usize;
        }
    }
    dist
}

fn maturity_distribution(conn: &Connection, deck_filter: Option<&str>) -> Vec<(String, usize)> {
    let deck_clause = if deck_filter.is_some() {
        "WHERE c.deck_id = (SELECT id FROM decks WHERE name = ?1)"
    } else {
        ""
    };

    let sql = format!(
        "SELECT c.stability, c.state FROM cards c {clause}",
        clause = deck_clause,
    );

    let mut buckets: BTreeMap<String, usize> = BTreeMap::new();
    buckets.insert("New".to_string(), 0);
    buckets.insert("< 1 day".to_string(), 0);
    buckets.insert("1-3 days".to_string(), 0);
    buckets.insert("3-7 days".to_string(), 0);
    buckets.insert("1-2 weeks".to_string(), 0);
    buckets.insert("2-4 weeks".to_string(), 0);
    buckets.insert("1-3 months".to_string(), 0);
    buckets.insert("> 3 months".to_string(), 0);

    let order = [
        "New",
        "< 1 day",
        "1-3 days",
        "3-7 days",
        "1-2 weeks",
        "2-4 weeks",
        "1-3 months",
        "> 3 months",
    ];

    let mut stmt = conn.prepare(&sql).unwrap();
    let rows: Vec<(f64, u8)> = if let Some(name) = deck_filter {
        stmt.query_map(params![name], |row| {
            Ok((row.get::<_, f64>(0)?, row.get::<_, u8>(1)?))
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    } else {
        stmt.query_map([], |row| Ok((row.get::<_, f64>(0)?, row.get::<_, u8>(1)?)))
            .unwrap()
            .filter_map(|r| r.ok())
            .collect()
    };

    for (stability, state) in rows {
        let bucket = if state == 0 {
            "New"
        } else if stability < 1.0 {
            "< 1 day"
        } else if stability < 3.0 {
            "1-3 days"
        } else if stability < 7.0 {
            "3-7 days"
        } else if stability < 14.0 {
            "1-2 weeks"
        } else if stability < 30.0 {
            "2-4 weeks"
        } else if stability < 90.0 {
            "1-3 months"
        } else {
            "> 3 months"
        };
        *buckets.get_mut(bucket).unwrap() += 1;
    }

    order
        .iter()
        .map(|k| (k.to_string(), *buckets.get(*k).unwrap_or(&0)))
        .collect()
}

// ── Printing ────────────────────────────────────────────────────────────

fn print_header(title: &str) {
    println!();
    println!("{BOLD}{CYAN}── {title} ─────────────────────────────────────────{RESET}");
    println!();
}

fn print_deck_overview(decks: &[DeckStats]) {
    print_header("Deck Overview");

    if decks.is_empty() {
        println!("  {DIM}No decks found.{RESET}");
        return;
    }

    let name_width = decks
        .iter()
        .map(|d| d.name.len())
        .max()
        .unwrap_or(10)
        .max(4);

    println!(
        "  {BOLD}{:<name_width$}  {:>5}  {:>5}  {:>5}  {:>5}  {:>5}  {:>5}{RESET}",
        "Deck",
        "Total",
        "New",
        "Lrn",
        "Rev",
        "ReLrn",
        "Due",
        name_width = name_width,
    );
    println!("  {DIM}{}{RESET}", "─".repeat(name_width + 38));

    let max_total = decks.iter().map(|d| d.total).max().unwrap_or(1);
    let bar_width = 30;

    for d in decks {
        let due_color = if d.due_today > 0 { YELLOW } else { DIM };
        println!(
            "  {:<name_width$}  {:>5}  {GREEN}{:>5}{RESET}  {YELLOW}{:>5}{RESET}  {CYAN}{:>5}{RESET}  {RED}{:>5}{RESET}  {due_color}{:>5}{RESET}",
            d.name,
            d.total,
            d.new,
            d.learning,
            d.review,
            d.relearning,
            d.due_today,
            name_width = name_width,
            due_color = due_color,
        );
        print!("  ");
        let new_w = if max_total > 0 {
            (d.new as f64 / max_total as f64 * bar_width as f64) as usize
        } else {
            0
        };
        let lrn_w = if max_total > 0 {
            (d.learning as f64 / max_total as f64 * bar_width as f64) as usize
        } else {
            0
        };
        let rev_w = if max_total > 0 {
            (d.review as f64 / max_total as f64 * bar_width as f64) as usize
        } else {
            0
        };
        let re_w = if max_total > 0 {
            (d.relearning as f64 / max_total as f64 * bar_width as f64) as usize
        } else {
            0
        };

        print!("{GREEN}", GREEN = GREEN);
        for _ in 0..new_w {
            print!("{BAR_FULL}");
        }
        print!("{YELLOW}", YELLOW = YELLOW);
        for _ in 0..lrn_w {
            print!("{BAR_FULL}");
        }
        print!("{CYAN}", CYAN = CYAN);
        for _ in 0..rev_w {
            print!("{BAR_FULL}");
        }
        print!("{RED}", RED = RED);
        for _ in 0..re_w {
            print!("{BAR_FULL}");
        }
        println!("{RESET}");
    }
}

fn print_forecast(buckets: &[ForecastBucket]) {
    print_header("Due Forecast");

    let max_count = buckets.iter().map(|b| b.count).max().unwrap_or(1);
    let label_width = buckets.iter().map(|b| b.label.len()).max().unwrap_or(10);
    let bar_width = 40;

    let colors = [RED, YELLOW, YELLOW, GREEN, GREEN, CYAN, DIM];

    for (i, bucket) in buckets.iter().enumerate() {
        let color = colors.get(i).unwrap_or(&DIM);
        let bar = render_bar(bucket.count, max_count, bar_width, color);
        println!(
            "  {:<label_width$}  {:>4}  {bar}",
            bucket.label,
            bucket.count,
            label_width = label_width,
        );
    }
}

fn print_activity(activity: &[ReviewDay]) {
    print_header("Review Activity (last 30 days)");

    if activity.is_empty() || activity.iter().all(|r| r.count == 0) {
        println!("  {DIM}No reviews yet.{RESET}");
        return;
    }

    let max_count = activity.iter().map(|r| r.count).max().unwrap_or(1);
    let bar_width = 40;

    let total: usize = activity.iter().map(|r| r.count).sum();
    let active_days = activity.iter().filter(|r| r.count > 0).count();
    let avg = if active_days > 0 {
        total as f64 / active_days as f64
    } else {
        0.0
    };

    println!("  {DIM}Total: {total}  |  Active days: {active_days}  |  Avg: {avg:.1}/day{RESET}");
    println!();

    for r in activity {
        let short_date = &r.date[5..];
        let color = if r.count == 0 {
            DIM
        } else if r.count as f64 > avg * 1.5 {
            GREEN
        } else if r.count as f64 > avg * 0.5 {
            CYAN
        } else {
            YELLOW
        };
        let bar = render_bar(r.count, max_count, bar_width, color);
        if r.count > 0 {
            println!("  {short_date}  {:>3}  {bar}", r.count);
        } else {
            println!("  {DIM}{short_date}    ·{RESET}");
        }
    }
}

fn print_ratings(dist: &[usize; 4]) {
    print_header("Rating Distribution");

    let total: usize = dist.iter().sum();
    if total == 0 {
        println!("  {DIM}No reviews yet.{RESET}");
        return;
    }

    let labels = ["Again", "Hard ", "Good ", "Easy "];
    let colors = [RED, YELLOW, GREEN, CYAN];
    let max_count = *dist.iter().max().unwrap_or(&1);
    let bar_width = 40;

    for (i, (&count, label)) in dist.iter().zip(labels.iter()).enumerate() {
        let pct = count as f64 / total as f64 * 100.0;
        let bar = render_bar(count, max_count, bar_width, colors[i]);
        println!(
            "  {color}{label}{RESET}  {:>4}  ({pct:>5.1}%)  {bar}",
            count,
            color = colors[i],
        );
    }
}

fn print_maturity(buckets: &[(String, usize)]) {
    print_header("Card Maturity (stability)");

    let total: usize = buckets.iter().map(|(_, c)| c).sum();
    if total == 0 {
        println!("  {DIM}No cards.{RESET}");
        return;
    }

    let max_count = buckets.iter().map(|(_, c)| *c).max().unwrap_or(1);
    let label_width = buckets.iter().map(|(l, _)| l.len()).max().unwrap_or(10);
    let bar_width = 40;

    let colors = [DIM, RED, RED, YELLOW, YELLOW, GREEN, CYAN, MAGENTA];

    for (i, (label, count)) in buckets.iter().enumerate() {
        if *count == 0 {
            continue;
        }
        let pct = *count as f64 / total as f64 * 100.0;
        let color = colors.get(i).unwrap_or(&DIM);
        let bar = render_bar(*count, max_count, bar_width, color);
        println!(
            "  {:<label_width$}  {:>4}  ({pct:>5.1}%)  {bar}",
            label,
            count,
            label_width = label_width,
        );
    }
}

// ── Public entry point ──────────────────────────────────────────────────

pub fn run(conn: &Connection, deck_filter: Option<&str>) {
    let title = match deck_filter {
        Some(name) => format!("turbo-m stats — {name}"),
        None => "turbo-m stats".to_string(),
    };
    println!("{BOLD}{MAGENTA}{title}{RESET}");

    let decks = deck_overview(conn, deck_filter);
    print_deck_overview(&decks);

    let forecast = due_forecast(conn, deck_filter);
    print_forecast(&forecast);

    let maturity = maturity_distribution(conn, deck_filter);
    print_maturity(&maturity);

    let activity = review_activity(conn, deck_filter, 30);
    print_activity(&activity);

    let ratings = rating_distribution(conn, deck_filter);
    print_ratings(&ratings);

    println!();
}
