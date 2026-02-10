# turbo-m Usage Guide

Non-interactive spaced repetition CLI powered by FSRS. All output is JSON.

```
turbo-m [--db <path>] <command>
```

`--db` overrides the database location (default: `~/.turbo-m.db`).

## Deck Management

```sh
# Create
turbo-m deck create --name "German A1"
turbo-m deck create --name "Japanese N5" --meta '{"lang":"ja","tags":["vocab"]}'

# List
turbo-m deck list

# Update (rename, metadata, FSRS params — any combination)
turbo-m deck update --name "German A1" --rename "Deutsch"
turbo-m deck update --name "Deutsch" --meta '{"lang":"de"}'
turbo-m deck update --name "Deutsch" --fsrs-params '{"w":[0.4,0.6,2.4,5.8]}'
```

## Importing Cards

Read `front##back` lines from a file, skipping duplicates by content match.

```sh
turbo-m import --deck "German" --file words.txt
# {"imported":13,"skipped":0,"total":13}

# Re-running is safe — duplicates are skipped
turbo-m import --deck "German" --file words.txt
# {"imported":0,"skipped":13,"total":13}

# Custom separator (default: ##)
turbo-m import --deck "German" --file words.tsv --separator $'\t'
```

Malformed lines (missing separator) are skipped with a warning on stderr.

## Adding Cards (JSON)

Card content is free-form JSON — turbo-m doesn't interpret it.

```sh
# Single card
turbo-m add --deck "German A1" --content '{"front":"Hund","back":"Dog"}'
# {"id":1}

# Batch via stdin (JSON array)
echo '[{"front":"Apfel","back":"Apple"},{"front":"Birne","back":"Pear"}]' \
  | turbo-m add --deck "German A1"
# [3,4]
```

> **Note:** `add` does not deduplicate. Use `import` for idempotent file loads.

## Fetching Cards

```sh
# Due cards (default limit: 12)
turbo-m fetch --deck "German A1"
turbo-m fetch --deck "German A1" --limit 20

# New (unreviewed) cards
turbo-m fetch --deck "German A1" --new --limit 10
```

Due cards are sorted by due date (most overdue first). New cards have `"state":0` and `"due":null`.

## Processing Reviews

Pipe review results via stdin. Each entry needs `card_id` and `rating`:

| Rating | Meaning | FSRS Effect |
|--------|---------|-------------|
| 1 | Again | Reset — short relearning interval |
| 2 | Hard | Longer interval, penalizes difficulty |
| 3 | Good | Standard progression |
| 4 | Easy | Bonus — longer interval, lower difficulty |

```sh
echo '[{"card_id":1,"rating":3},{"card_id":2,"rating":1}]' | turbo-m review
# {"processed":2,"status":"ok"}
```

## Statistics

```sh
# All decks
turbo-m stats

# Specific deck
turbo-m stats --deck "German A1"
```

Shows deck overview, due forecast, card maturity distribution, review activity (last 30 days), and rating distribution.

## Pipe Workflow

The intended pipeline: `turbo-m fetch → TUI → turbo-m review`

```sh
# Review due cards
turbo-m fetch --deck "German A1" | my-tui-app | turbo-m review

# Learn new cards
turbo-m fetch --deck "German A1" --new --limit 10 | my-tui-app | turbo-m review
```

## Library Usage

```rust
use turbo_m::TurboM;
use turbo_m::models::ReviewSubmission;
use std::path::Path;

let tm = TurboM::new(Path::new("/home/user/.turbo-m.db"))?;

let due_cards = tm.fetch_due("German A1", 12)?;
let new_cards = tm.fetch_new("German A1", 10)?;

let cards = vec![
    serde_json::json!({"front": "Hund", "back": "Dog"}),
    serde_json::json!({"front": "Katze", "back": "Cat"}),
];
let result = tm.import_cards("German A1", cards)?;

let reviews = vec![
    ReviewSubmission { card_id: 1, rating: 3 },
    ReviewSubmission { card_id: 2, rating: 1 },
];
tm.process_reviews(&reviews)?;
```

## Drill (Interactive TUI)

Typing drill using the same database. Shows card back as prompt, you type the front.

```
drill [--db <path>] --deck <name> [--limit <n>] [--new]
```

```sh
drill --deck "German A1"              # due cards
drill --deck "German A1" --new --limit 10  # new cards
```

### How it works

1. Cards are shuffled. Each card appears **3 times**.
2. Feedback: correct → silent, typo (Levenshtein ≤ 2) → blue, wrong → red (press Enter).
3. Wrong answers are re-queued until answered correctly enough times.
4. Only the **first attempt** per card is sent to FSRS: correct → 3 (Good), typo → 2 (Hard), wrong → 1 (Again).

### Normalization

Input is compared after stripping diacritics, punctuation, and whitespace, then lowercasing (`café` matches `Cafe`).

## Card States

| Value | State | Meaning |
|-------|-------|---------|
| 0 | New | Never reviewed |
| 1 | Learning | First reviews (short intervals) |
| 2 | Review | Graduated — normal schedule |
| 3 | Relearning | Lapsed — relearning |

## Database

Default: `~/.turbo-m.db` (SQLite).

```sh
sqlite3 ~/.turbo-m.db "SELECT id, content, state, due, stability, difficulty FROM cards;"
sqlite3 ~/.turbo-m.db "SELECT * FROM revlog ORDER BY review_date DESC LIMIT 10;"
```
