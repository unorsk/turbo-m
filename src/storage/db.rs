use rusqlite::Connection;

use crate::error::AppError;

/// Initialize the database schema. Uses CREATE TABLE IF NOT EXISTS so it's
/// safe to call on every startup.
pub fn init_db(conn: &Connection) -> Result<(), AppError> {
    // Enable foreign keys and set a busy timeout for concurrent access
    conn.execute_batch("PRAGMA foreign_keys = ON; PRAGMA busy_timeout = 5000;")?;

    conn.execute_batch(
        "
        CREATE TABLE IF NOT EXISTS decks (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL UNIQUE,
            metadata TEXT NOT NULL DEFAULT '{}',
            fsrs_params TEXT NOT NULL DEFAULT '{}',
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS cards (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            deck_id INTEGER NOT NULL REFERENCES decks(id) ON DELETE CASCADE,
            content TEXT NOT NULL,
            state INTEGER NOT NULL DEFAULT 0,
            due DATETIME,
            stability REAL NOT NULL DEFAULT 0.0,
            difficulty REAL NOT NULL DEFAULT 0.0,
            elapsed_days INTEGER NOT NULL DEFAULT 0,
            scheduled_days INTEGER NOT NULL DEFAULT 0,
            reps INTEGER NOT NULL DEFAULT 0,
            lapses INTEGER NOT NULL DEFAULT 0,
            last_review DATETIME,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );

        CREATE TABLE IF NOT EXISTS revlog (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            card_id INTEGER NOT NULL REFERENCES cards(id) ON DELETE CASCADE,
            rating INTEGER NOT NULL,
            state INTEGER NOT NULL,
            due DATETIME NOT NULL,
            stability REAL NOT NULL,
            difficulty REAL NOT NULL,
            review_date DATETIME DEFAULT CURRENT_TIMESTAMP
        );
        ",
    )?;

    Ok(())
}
