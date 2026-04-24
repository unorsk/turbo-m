#[cfg(feature = "sqlite")]
use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput, ValueRef};
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Error for converting a raw integer to an enum variant.
/// Wraps the descriptive message from `TryFrom` so it can be used with
/// `FromSqlError::Other`, which requires `std::error::Error`.
#[cfg(feature = "sqlite")]
#[derive(Debug)]
struct EnumConversionError(String);

#[cfg(feature = "sqlite")]
impl std::fmt::Display for EnumConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[cfg(feature = "sqlite")]
impl std::error::Error for EnumConversionError {}

/// Card scheduling state in the FSRS system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "u8", into = "u8")]
#[repr(u8)]
pub enum CardState {
    New = 0,
    Learning = 1,
    Review = 2,
    Relearning = 3,
}

impl TryFrom<u8> for CardState {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::New),
            1 => Ok(Self::Learning),
            2 => Ok(Self::Review),
            3 => Ok(Self::Relearning),
            _ => Err(format!(
                "Invalid card state: {value}. Must be 0 (New), 1 (Learning), 2 (Review), or 3 (Relearning)"
            )),
        }
    }
}

impl From<CardState> for u8 {
    fn from(s: CardState) -> Self {
        s as u8
    }
}

#[cfg(feature = "sqlite")]
impl FromSql for CardState {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
        let v = value.as_i64()?;
        let b = u8::try_from(v).map_err(|_| FromSqlError::OutOfRange(v))?;
        CardState::try_from(b)
            .map_err(|msg| FromSqlError::Other(Box::new(EnumConversionError(msg))))
    }
}

#[cfg(feature = "sqlite")]
impl ToSql for CardState {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        Ok(ToSqlOutput::from(*self as i64))
    }
}

/// Review rating (user assessment of recall quality).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "u8", into = "u8")]
#[repr(u8)]
pub enum Rating {
    Again = 1,
    Hard = 2,
    Good = 3,
    Easy = 4,
}

impl TryFrom<u8> for Rating {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Again),
            2 => Ok(Self::Hard),
            3 => Ok(Self::Good),
            4 => Ok(Self::Easy),
            _ => Err(format!(
                "Invalid rating: {value}. Must be 1 (Again), 2 (Hard), 3 (Good), or 4 (Easy)"
            )),
        }
    }
}

impl From<Rating> for u8 {
    fn from(r: Rating) -> Self {
        r as u8
    }
}

#[cfg(feature = "sqlite")]
impl FromSql for Rating {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
        let v = value.as_i64()?;
        let b = u8::try_from(v).map_err(|_| FromSqlError::OutOfRange(v))?;
        Rating::try_from(b).map_err(|msg| FromSqlError::Other(Box::new(EnumConversionError(msg))))
    }
}

#[cfg(feature = "sqlite")]
impl ToSql for Rating {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        Ok(ToSqlOutput::from(*self as i64))
    }
}

/// A deck groups cards together and can have its own FSRS parameters.
#[derive(Debug, Serialize, Deserialize)]
pub struct Deck {
    pub id: i64,
    pub name: String,
    pub metadata: Value,
    pub fsrs_params: Value,
}

/// What the CLI outputs when fetching cards (for the TUI to consume).
/// Hides internal FSRS math fields -- the TUI only needs content and scheduling info.
#[derive(Debug, Serialize, Deserialize)]
pub struct CardDTO {
    pub id: i64,
    pub deck_id: i64,
    pub content: Value,
    pub due: Option<String>,
    pub state: CardState,
}

/// What the TUI pipes back after a review session.
#[derive(Debug, Serialize, Deserialize)]
pub struct ReviewSubmission {
    pub card_id: i64,
    pub rating: Rating,
}

/// What the CLI outputs for the "hardest" query -- includes difficulty metrics.
#[derive(Debug, Serialize, Deserialize)]
pub struct HardestCardDTO {
    pub id: i64,
    pub deck_id: i64,
    pub content: Value,
    pub difficulty: f64,
    pub lapses: i32,
    pub reps: i32,
    pub state: CardState,
}

/// Result of an import operation (returned as JSON).
#[derive(Debug, Serialize, Deserialize)]
pub struct ImportResult {
    pub imported: usize,
    pub skipped: usize,
    pub total: usize,
}

/// Full card row from the database, used by the FSRS engine.
#[derive(Debug)]
pub struct CardRow {
    pub id: i64,
    pub deck_id: i64,
    pub content: String,
    pub state: CardState,
    pub due: Option<String>,
    pub stability: f64,
    pub difficulty: f64,
    pub elapsed_days: i64,
    pub scheduled_days: i64,
    pub reps: i32,
    pub lapses: i32,
    pub last_review: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn card_state_try_from_valid() {
        assert_eq!(CardState::try_from(0u8), Ok(CardState::New));
        assert_eq!(CardState::try_from(1u8), Ok(CardState::Learning));
        assert_eq!(CardState::try_from(2u8), Ok(CardState::Review));
        assert_eq!(CardState::try_from(3u8), Ok(CardState::Relearning));
    }

    #[test]
    fn card_state_try_from_invalid() {
        assert!(CardState::try_from(4u8).is_err());
        assert!(CardState::try_from(255u8).is_err());
    }

    #[test]
    fn card_state_u8_roundtrip() {
        for v in 0..=3u8 {
            let state = CardState::try_from(v).unwrap();
            assert_eq!(u8::from(state), v);
        }
    }

    #[test]
    fn card_state_serde_roundtrip() {
        for state in [
            CardState::New,
            CardState::Learning,
            CardState::Review,
            CardState::Relearning,
        ] {
            let json = serde_json::to_string(&state).unwrap();
            let deserialized: CardState = serde_json::from_str(&json).unwrap();
            assert_eq!(state, deserialized);
        }
    }

    #[test]
    fn card_state_serde_rejects_invalid() {
        assert!(serde_json::from_str::<CardState>("5").is_err());
        assert!(serde_json::from_str::<CardState>("255").is_err());
    }

    #[test]
    fn rating_try_from_valid() {
        assert_eq!(Rating::try_from(1u8), Ok(Rating::Again));
        assert_eq!(Rating::try_from(2u8), Ok(Rating::Hard));
        assert_eq!(Rating::try_from(3u8), Ok(Rating::Good));
        assert_eq!(Rating::try_from(4u8), Ok(Rating::Easy));
    }

    #[test]
    fn rating_try_from_invalid() {
        assert!(Rating::try_from(0u8).is_err());
        assert!(Rating::try_from(5u8).is_err());
    }

    #[test]
    fn rating_u8_roundtrip() {
        for v in 1..=4u8 {
            let rating = Rating::try_from(v).unwrap();
            assert_eq!(u8::from(rating), v);
        }
    }

    #[test]
    fn rating_serde_roundtrip() {
        for rating in [Rating::Again, Rating::Hard, Rating::Good, Rating::Easy] {
            let json = serde_json::to_string(&rating).unwrap();
            let deserialized: Rating = serde_json::from_str(&json).unwrap();
            assert_eq!(rating, deserialized);
        }
    }

    #[test]
    fn rating_serde_rejects_invalid() {
        assert!(serde_json::from_str::<Rating>("0").is_err());
        assert!(serde_json::from_str::<Rating>("5").is_err());
    }

    #[test]
    fn review_submission_deserializes_with_valid_rating() {
        let json = r#"{"card_id": 1, "rating": 3}"#;
        let sub: ReviewSubmission = serde_json::from_str(json).unwrap();
        assert_eq!(sub.card_id, 1);
        assert_eq!(sub.rating, Rating::Good);
    }

    #[test]
    fn review_submission_rejects_invalid_rating() {
        let json = r#"{"card_id": 1, "rating": 0}"#;
        assert!(serde_json::from_str::<ReviewSubmission>(json).is_err());

        let json = r#"{"card_id": 1, "rating": 5}"#;
        assert!(serde_json::from_str::<ReviewSubmission>(json).is_err());
    }

    #[test]
    fn card_dto_state_serializes_as_integer() {
        let dto = CardDTO {
            id: 1,
            deck_id: 1,
            content: serde_json::json!({}),
            due: None,
            state: CardState::Learning,
        };
        let json = serde_json::to_value(&dto).unwrap();
        assert_eq!(json["state"], 1);
    }
}
