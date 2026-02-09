use thiserror::Error;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("Database error: {0}")]
    Db(#[from] rusqlite::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Deck not found: {0}")]
    DeckNotFound(String),

    #[error("Card not found: {0}")]
    CardNotFound(i64),

    #[error("Invalid rating: {0}. Must be 1 (Again), 2 (Hard), 3 (Good), or 4 (Easy)")]
    InvalidRating(u32),
}
