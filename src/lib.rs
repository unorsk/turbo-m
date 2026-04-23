pub mod cli;
pub mod client;
pub mod engine;
pub mod error;
pub mod model;
pub mod storage;

pub use model as models;

use std::path::Path;

use rusqlite::Connection;
use serde_json::Value;

use error::AppError;
use model::{CardDTO, Deck, HardestCardDTO, ImportResult, ReviewSubmission};

/// Main entry point for the turbo-m library (local SQLite backend).
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

    pub fn add_card(&self, deck_name: &str, content: Value) -> Result<i64, AppError> {
        storage::repo::add_card(&self.conn, deck_name, content)
    }

    pub fn add_cards(&self, deck_name: &str, contents: Vec<Value>) -> Result<Vec<i64>, AppError> {
        storage::repo::add_cards(&self.conn, deck_name, contents)
    }

    pub fn import_cards(
        &self,
        deck_name: &str,
        contents: Vec<Value>,
    ) -> Result<ImportResult, AppError> {
        storage::repo::import_cards(&self.conn, deck_name, contents)
    }

    pub fn fetch_due(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        storage::repo::fetch_due(&self.conn, deck_name, limit)
    }

    pub fn count_due(&self, deck_name: &str) -> Result<u32, AppError> {
        storage::repo::count_due(&self.conn, deck_name)
    }

    pub fn fetch_new(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        storage::repo::fetch_new(&self.conn, deck_name, limit)
    }

    pub fn fetch_hardest(
        &self,
        deck_name: &str,
        limit: u32,
    ) -> Result<Vec<HardestCardDTO>, AppError> {
        storage::repo::fetch_hardest(&self.conn, deck_name, limit)
    }

    pub fn process_reviews(&self, reviews: &[ReviewSubmission]) -> Result<(), AppError> {
        storage::repo::process_reviews(&self.conn, reviews)
    }
}

/// Dispatch enum for local vs. remote backends.
/// All data operations go through this so the CLI and drill module
/// work identically regardless of backend.
pub enum Backend {
    Local(TurboM),
    Remote(client::RemoteClient),
}

impl Backend {
    pub fn create_deck(&self, name: &str, metadata: Value) -> Result<Deck, AppError> {
        match self {
            Backend::Local(tm) => tm.create_deck(name, metadata),
            Backend::Remote(c) => c.create_deck(name, metadata),
        }
    }

    pub fn list_decks(&self) -> Result<Vec<Deck>, AppError> {
        match self {
            Backend::Local(tm) => tm.list_decks(),
            Backend::Remote(c) => c.list_decks(),
        }
    }

    pub fn update_deck(
        &self,
        name: &str,
        new_name: Option<&str>,
        metadata: Option<Value>,
        fsrs_params: Option<Value>,
    ) -> Result<Deck, AppError> {
        match self {
            Backend::Local(tm) => tm.update_deck(name, new_name, metadata, fsrs_params),
            Backend::Remote(c) => c.update_deck(name, new_name, metadata, fsrs_params),
        }
    }

    pub fn add_card(&self, deck_name: &str, content: Value) -> Result<i64, AppError> {
        match self {
            Backend::Local(tm) => tm.add_card(deck_name, content),
            Backend::Remote(c) => c.add_card(deck_name, content),
        }
    }

    pub fn add_cards(&self, deck_name: &str, contents: Vec<Value>) -> Result<Vec<i64>, AppError> {
        match self {
            Backend::Local(tm) => tm.add_cards(deck_name, contents),
            Backend::Remote(c) => c.add_cards(deck_name, contents),
        }
    }

    pub fn import_cards(
        &self,
        deck_name: &str,
        contents: Vec<Value>,
    ) -> Result<ImportResult, AppError> {
        match self {
            Backend::Local(tm) => tm.import_cards(deck_name, contents),
            Backend::Remote(c) => c.import_cards(deck_name, contents),
        }
    }

    pub fn fetch_due(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        match self {
            Backend::Local(tm) => tm.fetch_due(deck_name, limit),
            Backend::Remote(c) => c.fetch_due(deck_name, limit),
        }
    }

    pub fn count_due(&self, deck_name: &str) -> Result<u32, AppError> {
        match self {
            Backend::Local(tm) => tm.count_due(deck_name),
            Backend::Remote(c) => c.count_due(deck_name),
        }
    }

    pub fn fetch_new(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        match self {
            Backend::Local(tm) => tm.fetch_new(deck_name, limit),
            Backend::Remote(c) => c.fetch_new(deck_name, limit),
        }
    }

    pub fn fetch_hardest(
        &self,
        deck_name: &str,
        limit: u32,
    ) -> Result<Vec<HardestCardDTO>, AppError> {
        match self {
            Backend::Local(tm) => tm.fetch_hardest(deck_name, limit),
            Backend::Remote(c) => c.fetch_hardest(deck_name, limit),
        }
    }

    pub fn process_reviews(&self, reviews: &[ReviewSubmission]) -> Result<(), AppError> {
        match self {
            Backend::Local(tm) => tm.process_reviews(reviews),
            Backend::Remote(c) => c.process_reviews(reviews),
        }
    }
}
