use serde::{Deserialize, Serialize};
use serde_json::Value;

/// A deck groups cards together and can have its own FSRS parameters.
#[derive(Debug, Serialize, Deserialize)]
pub struct Deck {
    pub id: i64,
    pub name: String,
    pub metadata: Value,
    pub fsrs_params: Value,
}

/// What the CLI outputs when fetching cards (for the TUI to consume).
/// Hides internal FSRS math fields — the TUI only needs content and scheduling info.
#[derive(Debug, Serialize, Deserialize)]
pub struct CardDTO {
    pub id: i64,
    pub deck_id: i64,
    pub content: Value,
    pub due: Option<String>,
    pub state: u8, // 0=New, 1=Learning, 2=Review, 3=Relearning
}

/// What the TUI pipes back after a review session.
#[derive(Debug, Serialize, Deserialize)]
pub struct ReviewSubmission {
    pub card_id: i64,
    pub rating: u32, // 1=Again, 2=Hard, 3=Good, 4=Easy
}

/// Result of an import operation (returned as JSON).
#[derive(Debug, Serialize, Deserialize)]
pub struct ImportResult {
    pub imported: usize,
    pub skipped: usize,
    pub total: usize,
}

/// Internal: full card row from the database.
/// Not exposed in the public API.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct CardRow {
    pub id: i64,
    pub deck_id: i64,
    pub content: String,
    pub state: u8,
    pub due: Option<String>,
    pub stability: f64,
    pub difficulty: f64,
    pub elapsed_days: i64,
    pub scheduled_days: i64,
    pub reps: i32,
    pub lapses: i32,
    pub last_review: Option<String>,
}
