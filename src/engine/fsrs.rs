use chrono::{DateTime, Utc};
use rs_fsrs::{Card as FsrsCard, FSRS, Rating, State};

use crate::error::AppError;
use crate::model::CardRow;

/// Create an FSRS scheduler. For now uses default parameters;
/// deck-specific params can be added later by deserializing fsrs_params JSON.
pub fn make_fsrs() -> FSRS {
    FSRS::default()
}

/// Convert a user-provided integer (1-4) to an rs-fsrs Rating.
pub fn rating_from_u32(r: u32) -> Result<Rating, AppError> {
    match r {
        1 => Ok(Rating::Again),
        2 => Ok(Rating::Hard),
        3 => Ok(Rating::Good),
        4 => Ok(Rating::Easy),
        _ => Err(AppError::InvalidRating(r)),
    }
}

/// Map rs-fsrs State to the integer stored in the database.
pub fn state_to_u8(s: State) -> u8 {
    match s {
        State::New => 0,
        State::Learning => 1,
        State::Review => 2,
        State::Relearning => 3,
    }
}

/// Map a database integer back to rs-fsrs State.
pub fn u8_to_state(s: u8) -> State {
    match s {
        1 => State::Learning,
        2 => State::Review,
        3 => State::Relearning,
        _ => State::New,
    }
}

/// Convert an internal CardRow (from the DB) into an rs-fsrs Card
/// so we can feed it to the scheduler.
pub(crate) fn card_row_to_fsrs(row: &CardRow) -> FsrsCard {
    let due = row
        .due
        .as_ref()
        .and_then(|d| DateTime::parse_from_rfc3339(d).ok())
        .map(|d| d.with_timezone(&Utc))
        .unwrap_or_else(Utc::now);

    let last_review = row
        .last_review
        .as_ref()
        .and_then(|d| DateTime::parse_from_rfc3339(d).ok())
        .map(|d| d.with_timezone(&Utc))
        .unwrap_or_else(Utc::now);

    FsrsCard {
        due,
        stability: row.stability,
        difficulty: row.difficulty,
        elapsed_days: row.elapsed_days,
        scheduled_days: row.scheduled_days,
        reps: row.reps,
        lapses: row.lapses,
        state: u8_to_state(row.state),
        last_review,
    }
}
