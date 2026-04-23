use chrono::{DateTime, Utc};
use rs_fsrs::{Card as FsrsCard, FSRS, Rating as FsrsRating, State as FsrsState};

use crate::model::{CardRow, CardState, Rating};

/// Create an FSRS scheduler with default parameters.
pub fn make_fsrs() -> FSRS {
    FSRS::default()
}

/// Convert our Rating to an rs-fsrs Rating.
pub fn to_fsrs_rating(r: Rating) -> FsrsRating {
    match r {
        Rating::Again => FsrsRating::Again,
        Rating::Hard => FsrsRating::Hard,
        Rating::Good => FsrsRating::Good,
        Rating::Easy => FsrsRating::Easy,
    }
}

impl From<FsrsState> for CardState {
    fn from(s: FsrsState) -> Self {
        match s {
            FsrsState::New => CardState::New,
            FsrsState::Learning => CardState::Learning,
            FsrsState::Review => CardState::Review,
            FsrsState::Relearning => CardState::Relearning,
        }
    }
}

fn to_fsrs_state(s: CardState) -> FsrsState {
    match s {
        CardState::New => FsrsState::New,
        CardState::Learning => FsrsState::Learning,
        CardState::Review => FsrsState::Review,
        CardState::Relearning => FsrsState::Relearning,
    }
}

/// Convert a CardRow (from the DB) into an rs-fsrs Card
/// so we can feed it to the scheduler.
pub fn card_row_to_fsrs(row: &CardRow) -> FsrsCard {
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
        state: to_fsrs_state(row.state),
        last_review,
    }
}
