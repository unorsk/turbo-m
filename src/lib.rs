pub mod cli;
pub mod engine;
pub mod error;
pub mod model;
pub mod storage;

pub use model as models;

use std::path::Path;

use rusqlite::Connection;
use serde_json::Value;

use error::AppError;
use model::{CardDTO, Deck, ImportResult, ReviewSubmission};

/// Main entry point for the turbo-m library.
///
/// Wraps a SQLite connection and exposes all spaced-repetition operations.
/// Can be used both from the CLI binary and as a library by a TUI/web frontend.
pub struct TurboM {
    conn: Connection,
}

impl TurboM {
    /// Open (or create) a database at the given path and initialize the schema.
    pub fn new(db_path: &Path) -> Result<Self, AppError> {
        let conn = Connection::open(db_path)?;
        storage::db::init_db(&conn)?;
        Ok(Self { conn })
    }

    pub fn conn(&self) -> &Connection {
        &self.conn
    }

    /// Open an in-memory database (useful for testing).
    pub fn in_memory() -> Result<Self, AppError> {
        let conn = Connection::open_in_memory()?;
        storage::db::init_db(&conn)?;
        Ok(Self { conn })
    }

    // ── Deck operations ──────────────────────────────────────────────────

    pub fn create_deck(&self, name: &str, metadata: Value) -> Result<Deck, AppError> {
        storage::repo::create_deck(&self.conn, name, metadata)
    }

    pub fn list_decks(&self) -> Result<Vec<Deck>, AppError> {
        storage::repo::list_decks(&self.conn)
    }

    pub fn update_deck(
        &self,
        name: &str,
        new_name: Option<&str>,
        metadata: Option<Value>,
        fsrs_params: Option<Value>,
    ) -> Result<Deck, AppError> {
        storage::repo::update_deck(&self.conn, name, new_name, metadata, fsrs_params)
    }

    // ── Card operations ──────────────────────────────────────────────────

    /// Add a single card to a deck. Returns the new card's ID.
    pub fn add_card(&self, deck_name: &str, content: Value) -> Result<i64, AppError> {
        storage::repo::add_card(&self.conn, deck_name, content)
    }

    /// Add multiple cards to a deck in a single transaction. Returns their IDs.
    pub fn add_cards(&self, deck_name: &str, contents: Vec<Value>) -> Result<Vec<i64>, AppError> {
        storage::repo::add_cards(&self.conn, deck_name, contents)
    }

    /// Import cards into a deck, skipping duplicates (matched by content JSON).
    pub fn import_cards(
        &self,
        deck_name: &str,
        contents: Vec<Value>,
    ) -> Result<ImportResult, AppError> {
        storage::repo::import_cards(&self.conn, deck_name, contents)
    }

    /// Fetch cards that are due for review (state != New, due <= now).
    pub fn fetch_due(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        storage::repo::fetch_due(&self.conn, deck_name, limit)
    }

    /// Count cards that are due for review in a deck.
    pub fn count_due(&self, deck_name: &str) -> Result<u32, AppError> {
        storage::repo::count_due(&self.conn, deck_name)
    }

    /// Fetch new (never-reviewed) cards for a warm-up session.
    pub fn fetch_new(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        storage::repo::fetch_new(&self.conn, deck_name, limit)
    }

    // ── Review operations ────────────────────────────────────────────────

    /// Process review results from the TUI. Updates FSRS state and writes to revlog.
    pub fn process_reviews(&self, reviews: &[ReviewSubmission]) -> Result<(), AppError> {
        storage::repo::process_reviews(&self.conn, reviews)
    }
}
