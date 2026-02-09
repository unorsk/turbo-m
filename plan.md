turbo-m Implementation Plan v2

1. Project Scaffolding & Dependencies
[ ] Initialize project and update Cargo.toml.
[ ] Improvement: Add anyhow for app-level error handling, directories for standard paths, and rusqlite_migration for schema management.

Ini, TOML


[package]
name = "turbo-m"
version = "0.1.0"
edition = "2021"

[dependencies]
# Core Logic
fsrs = "1.0"
strsim = "0.11"
rand = "0.8"
chrono = { version = "0.4", features = ["serde"] }
uuid = { version = "1", features = ["v4", "serde"] }
unicode-normalization = "0.1"

# Database & Storage
rusqlite = { version = "0.31", features = ["bundled"] }
rusqlite_migration = "1.1" # Manages DB schema versions
serde = { version = "1", features = ["derive"] }
serde_json = "1"
directories = "5" # Cross-platform config paths

# CLI
rustyline = "14"
anyhow = "1"    # Easier error bubbling
thiserror = "1" # Typed errors for libraries

# Web
actix-web = "4"
actix-files = "0.6"
open = "5"      # To open browser automatically


[ ] Create directory structure:
Plaintext
src/
├── main.rs          # Args parsing, setup
├── error.rs         # Custom AppError enum (impl From<rusqlite::Error>, etc.)
├── model/
│   ├── mod.rs
│   ├── card.rs      # Card struct, CardState
│   └── review.rs    # ReviewLog struct (New!)
├── storage/
│   ├── mod.rs
│   ├── db.rs        # Connection setup, migrations
│   └── repo.rs      # High-level CRUD (get_due, save_review)
├── engine/
│   ├── mod.rs
│   ├── fsrs.rs      # Wrapper for scheduling logic
│   ├── grading.rs   # Levenshtein logic
│   └── session.rs   # The drill loop state machine
├── interface/
│   ├── mod.rs
│   ├── cli.rs       # Terminal UI
│   ├── tts.rs       # System TTS wrapper
│   └── web.rs       # Actix server
└── import.rs        # File parsing


Success Criteria: cargo check passes.
2. Data Models (src/model/)
[ ] Card Struct:
Rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Card {
    pub id: String,
    pub front: String,
    pub back: String,
    pub state: fsrs::State, // Use fsrs crate's State directly if compatible, else map
    pub stability: f64,
    pub difficulty: f64,
    pub elapsed_days: u64,
    pub scheduled_days: u64,
    pub due: DateTime<Utc>,
    pub last_review: Option<DateTime<Utc>>,
    pub lapses: u32,
    pub reps: u32,
}


[ ] ReviewLog Struct (New):
This is required to optimize FSRS later.
Rust
pub struct ReviewLog {
    pub id: i64,          // Auto-increment
    pub card_id: String,
    pub rating: i32,      // 1=Again, 2=Hard, 3=Good, 4=Easy
    pub state: i32,       // State *before* review
    pub due: DateTime<Utc>,
    pub stability: f64,
    pub difficulty: f64,
    pub elapsed_days: u64,
    pub last_review: DateTime<Utc>, // Time of this review
    pub duration: u64,    // Time taken to answer (ms)
}


Tests:
[ ] test_card_defaults: Ensure new cards start with stability 0.0.
3. Storage Layer (src/storage/)
[ ] Migrations (db.rs):
Define schema using rusqlite_migration::Migrations.
SQL
-- M1: Initial Schema
CREATE TABLE cards (
    id TEXT PRIMARY KEY,
    front TEXT NOT NULL,
    back TEXT NOT NULL,
    state INTEGER NOT NULL,
    stability REAL NOT NULL,
    difficulty REAL NOT NULL,
    elapsed_days INTEGER NOT NULL,
    scheduled_days INTEGER NOT NULL,
    due TEXT NOT NULL,
    last_review TEXT,
    lapses INTEGER NOT NULL,
    reps INTEGER NOT NULL,
    created_at TEXT NOT NULL
);

CREATE TABLE revlog (
    id INTEGER PRIMARY KEY,
    card_id TEXT NOT NULL,
    rating INTEGER NOT NULL,
    state INTEGER NOT NULL,
    due TEXT NOT NULL,
    stability REAL NOT NULL,
    difficulty REAL NOT NULL,
    elapsed_days INTEGER NOT NULL,
    last_review TEXT NOT NULL,
    duration INTEGER NOT NULL,
    FOREIGN KEY(card_id) REFERENCES cards(id)
);


[ ] Repository (repo.rs):
add_card(&Card)
update_card_and_log(&Card, &ReviewLog) (Transaction: must save card state AND insert log entry atomically).
get_queue_new(limit)
get_queue_review(limit)
get_bonus_candidates(limit) (Logic: High difficulty OR Recent Lapses).
Tests:
[ ] test_migration: Run migrations on in-memory DB.
[ ] test_transaction: Fail a log insert, ensure card update is rolled back.
4. FSRS Engine (src/engine/fsrs.rs)
[ ] Config: Load FSRS weights from a JSON file (or use defaults if missing).
[ ] Scheduling:
Map internal Card to fsrs::Card.
Calculate fsrs.next().
Critical: Return both the updated Card and the ReviewLog entry to be saved.
Tests:
[ ] test_next_interval: Verify a "Good" rating increases the interval.
[ ] test_retention_check: Verify "Again" resets stability/interval correctly.
5. Grading & Normalization (src/engine/grading.rs)
[ ] Normalization:
Standardize logic: NFD normalization -> Remove Diacritics (optional, maybe keep for language learning? Decision: Keep diacritics for strict mode, remove for loose) -> Lowercase -> Trim.
[ ] Levenshtein Logic:
fn grade(input: &str, target: &str) -> Grade.
Refinement:
Length < 3: Exact match required (No typos allowed).
Length 3-5: Max 1 distance.
Length > 5: Max 2 distance.
Tests:
[ ] test_short_words: "is" vs "it" should be WRONG, not TYPO.
6. The Session Loop (src/engine/session.rs)
This is the core "Drill" logic.
[ ] Session Item:
Rust
struct SessionItem {
    card: Card,
    mode: SessionMode, // Warmup vs Refresh
    success_streak: u8, // Target: 3
    fsrs_updated: bool, // True once the "First Attempt" is recorded
}


[ ] Logic:
If fsrs_updated == false:
Grade input.
Calculate FSRS memory update.
Create ReviewLog.
COMMIT to DB immediately. (Don't wait for session end, prevent data loss on crash).
Set fsrs_updated = true.
If fsrs_updated == true:
Grade input (purely for the drill loop).
Update success_streak.
Tests:
[ ] test_db_commit_order: Ensure FSRS state is saved after the first attempt, not the last.
7. Interactive CLI (src/interface/cli.rs)
[ ] Setup: Use directories crate to find config folder.
Mac: ~/Library/Application Support/turbo-m/
Linux: ~/.local/share/turbo-m/
[ ] Menu:
Warmup (New Cards)
Refresh (Due Cards)
Bonus Training (Web)
Stats
[ ] Drill UI:
Clear screen between cards.
Display "New Word" or "Review".
Input loop.
Typo Handling: If Grade::Typo, prompt: "[Input] vs [Target]. Was this a typo? (y/n)".
If y: Treat as Correct for FSRS, but reset drill streak to 0 (Strict Drill).
8. Web Interface (src/interface/web.rs)
[ ] State: Pass Arc<Mutex<Connection>> to Actix data.
[ ] Handler: GET /bonus-batch.
Logic: Fetch 15 "Leeches" (Difficulty > 7) or "Lapses" (Relearning).
If none, fetch random.
[ ] Template: Use raw HTML string with HTMX script tag (no need for complex template engine dependencies).
[ ] Concurrency:
When option 4 is chosen in CLI:
std::thread::spawn the Actix system.
Use open::that("http://localhost:8080") to launch browser.
Note: The CLI might need to block or wait for Ctrl-C to kill the server.
9. Implementation Order & Checklist
Phase 1: The Core
[ ] 1. Scaffold cargo new, add deps (anyhow, rusqlite, fsrs).
[ ] 2. Implement storage/db.rs with rusqlite_migration.
[ ] 3. Implement model/card.rs and storage/repo.rs.
[ ] 4. Write import.rs to populate DB from ## files.
[ ] Verification: Write a test that initializes DB and imports a file.
Phase 2: The Brain
[ ] 5. Implement grading.rs (Levenshtein).
[ ] 6. Implement engine/fsrs.rs (Scheduling wrapper).
[ ] 7. Implement engine/session.rs (The Drill Logic).
[ ] Verification: Unit tests for Grading thresholds and Session streak logic.
Phase 3: The Interface
[ ] 8. Implement interface/tts.rs (Platform check: cfg(target_os = "macos") -> Command::new("say")).
[ ] 9. Build interface/cli.rs (Rustyline loop).
[ ] 10. Wire up main.rs.
[ ] Verification: Play a full session in terminal. Check revlog table content.
Phase 4: The Bonus
[ ] 11. Implement interface/web.rs (Actix + HTMX).
[ ] 12. Add GET / and POST /check.
[ ] Verification: Run browser session, ensure it does not alter revlog or card schedules.
10. Edge Case & Failure Plan
Database Lock: Since Actix and CLI share the DB, set SQLite busy timeout to 5000ms (PRAGMA busy_timeout = 5000;).
Crash Recovery: Since we commit FSRS updates immediately on the first attempt (inside the loop), a crash mid-session is safe. The user just loses their "drill streak" for that specific word, but the SRS interval is already saved.
TTS Failure: If say command fails (e.g., linux), log error silently and continue. Don't panic.
