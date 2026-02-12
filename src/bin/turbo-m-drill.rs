use std::path::PathBuf;

use clap::Parser;
use rustyline::DefaultEditor;
use unicode_normalization::UnicodeNormalization;

use turbo_m::TurboM;
use turbo_m::models::{CardDTO, ReviewSubmission};

const BLUE: &str = "\x1b[34m";
const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

#[derive(Parser)]
#[command(name = "turbo-m-drill", about = "Spaced repetition drill TUI")]
struct Cli {
    /// Path to the SQLite database file (default: ~/.turbo-m.db)
    #[arg(long)]
    db: Option<PathBuf>,

    /// Deck name to drill
    #[arg(long)]
    deck: String,

    /// Maximum number of cards to fetch
    #[arg(long, default_value = "12")]
    limit: u32,

    /// Drill new (unreviewed) cards instead of due cards
    #[arg(long)]
    new: bool,
}

fn normalize(s: &str) -> String {
    s.nfkd()
        .filter(|c| !c.is_ascii_punctuation() && !c.is_whitespace())
        .collect::<String>()
        .to_lowercase()
}

fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (m, n) = (a.len(), b.len());
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) {
        *val = j;
    }
    for i in 1..=m {
        for j in 1..=n {
            if a[i - 1] == b[j - 1] {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = 1 + dp[i - 1][j].min(dp[i][j - 1]).min(dp[i - 1][j - 1]);
            }
        }
    }
    dp[m][n]
}

struct DrillCard {
    card: CardDTO,
    front: String,
    back: String,
}

fn extract_front_back(card: &CardDTO) -> (String, String) {
    let front = card
        .content
        .get("front")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let back = card
        .content
        .get("back")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    (front, back)
}

#[derive(Clone, Copy, PartialEq)]
enum AttemptResult {
    Correct,
    Typo,
    Wrong,
    Empty,
}

fn default_db_path() -> PathBuf {
    dirs::home_dir()
        .expect("Could not determine home directory")
        .join(".turbo-m.db")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let db_path = cli.db.unwrap_or_else(default_db_path);
    let tm = TurboM::new(&db_path)?;

    let cards = if cli.new {
        tm.fetch_new(&cli.deck, cli.limit)?
    } else {
        tm.fetch_due(&cli.deck, cli.limit)?
    };

    if cards.is_empty() {
        eprintln!("No cards to drill.");
        return Ok(());
    }

    let mut queue: Vec<DrillCard> = cards
        .into_iter()
        .map(|c| {
            let (front, back) = extract_front_back(&c);
            DrillCard {
                card: c,
                front,
                back,
            }
        })
        .collect();

    use rand::seq::SliceRandom;
    let mut rng = rand::rng();
    queue.shuffle(&mut rng);

    let original_len = queue.len();
    let mut duplicated: Vec<DrillCard> = Vec::with_capacity(original_len);
    for dc in &queue {
        duplicated.push(DrillCard {
            card: CardDTO {
                id: dc.card.id,
                deck_id: dc.card.deck_id,
                content: dc.card.content.clone(),
                due: dc.card.due.clone(),
                state: dc.card.state,
            },
            front: dc.front.clone(),
            back: dc.back.clone(),
        });
    }
    queue.append(&mut duplicated);

    // correct_counts tracks how many correct answers per card_id
    let mut correct_counts: std::collections::HashMap<i64, u32> = std::collections::HashMap::new();
    // first_attempt tracks the result of the very first attempt per card_id for FSRS
    let mut first_attempt: std::collections::HashMap<i64, AttemptResult> =
        std::collections::HashMap::new();

    let mut rl = DefaultEditor::new()?;
    let mut idx = 0;

    while idx < queue.len() {
        let dc = &queue[idx];
        let prompt = format!("{}> ", dc.back);

        let input = match rl.readline(&prompt) {
            Ok(line) => line,
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => {
                break;
            }
            Err(e) => return Err(e.into()),
        };

        let normalized_input = normalize(&input);
        let normalized_word = normalize(&dc.front);
        let card_id = dc.card.id;

        let result = if normalized_input.is_empty() {
            AttemptResult::Empty
        } else if normalized_input == normalized_word {
            AttemptResult::Correct
        } else {
            let dist = levenshtein(&normalized_input, &normalized_word);
            if dist <= 2 {
                AttemptResult::Typo
            } else {
                AttemptResult::Wrong
            }
        };

        first_attempt.entry(card_id).or_insert(result);

        match result {
            AttemptResult::Correct => {
                *correct_counts.entry(card_id).or_insert(0) += 1;
                if correct_counts[&card_id] < 2 {
                    queue.push(DrillCard {
                        card: CardDTO {
                            id: dc.card.id,
                            deck_id: dc.card.deck_id,
                            content: dc.card.content.clone(),
                            due: dc.card.due.clone(),
                            state: dc.card.state,
                        },
                        front: dc.front.clone(),
                        back: dc.back.clone(),
                    });
                }
            }
            AttemptResult::Typo => {
                println!("{BLUE}{}{RESET}", dc.front);
                *correct_counts.entry(card_id).or_insert(0) += 1;
                if correct_counts[&card_id] < 2 {
                    queue.push(DrillCard {
                        card: CardDTO {
                            id: dc.card.id,
                            deck_id: dc.card.deck_id,
                            content: dc.card.content.clone(),
                            due: dc.card.due.clone(),
                            state: dc.card.state,
                        },
                        front: dc.front.clone(),
                        back: dc.back.clone(),
                    });
                }
            }
            AttemptResult::Wrong | AttemptResult::Empty => {
                println!("{RED}{}{RESET}", dc.front);
                let _ = rl.readline("");
                queue.push(DrillCard {
                    card: CardDTO {
                        id: dc.card.id,
                        deck_id: dc.card.deck_id,
                        content: dc.card.content.clone(),
                        due: dc.card.due.clone(),
                        state: dc.card.state,
                    },
                    front: dc.front.clone(),
                    back: dc.back.clone(),
                });
            }
        }
        idx += 1;
    }

    // Build FSRS reviews from first attempts only
    let reviews: Vec<ReviewSubmission> = first_attempt
        .into_iter()
        .map(|(card_id, result)| {
            let rating = match result {
                AttemptResult::Empty => 1,
                AttemptResult::Wrong => 2,
                AttemptResult::Typo => 3,
                AttemptResult::Correct => 4,
            };
            ReviewSubmission { card_id, rating }
        })
        .collect();

    if !reviews.is_empty() {
        tm.process_reviews(&reviews)?;
        eprintln!("Processed {} reviews.", reviews.len());
    }

    eprintln!("Done!");
    Ok(())
}
