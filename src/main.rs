use std::io::Read;
use std::path::PathBuf;

use clap::{Parser, Subcommand};

use turbo_m::TurboM;
use turbo_m::models::ReviewSubmission;

#[derive(Parser)]
#[command(name = "turbo-m", about = "Spaced repetition CLI using FSRS")]
struct Cli {
    /// Path to the SQLite database file (default: ~/.turbo-m.db)
    #[arg(long)]
    db: Option<PathBuf>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Manage decks
    Deck {
        #[command(subcommand)]
        action: DeckAction,
    },
    /// Add cards to a deck (single via --content, batch via stdin)
    Add {
        /// Deck name
        #[arg(long)]
        deck: String,
        /// Card content as JSON (omit to read JSON array from stdin)
        #[arg(long)]
        content: Option<String>,
    },
    /// Fetch cards from a deck
    Fetch {
        /// Deck name
        #[arg(long)]
        deck: String,
        /// Maximum number of cards to return
        #[arg(long, default_value = "12")]
        limit: u32,
        /// Fetch new (unreviewed) cards instead of due cards
        #[arg(long)]
        new: bool,
    },
    /// Import cards from a ##-delimited file (front##back per line), skipping duplicates
    Import {
        /// Deck name
        #[arg(long)]
        deck: String,
        /// Path to the file to import
        #[arg(long)]
        file: PathBuf,
        /// Field separator between front and back
        #[arg(long, default_value = "##")]
        separator: String,
    },
    /// Process review results (reads JSON array of {card_id, rating} from stdin)
    Review,
    /// Show learning statistics
    Stats {
        /// Show stats for a specific deck only
        #[arg(long)]
        deck: Option<String>,
    },
    /// Show GitHub-style contribution tiles
    Tiles {
        /// Show tiles for a specific deck only
        #[arg(long)]
        deck: Option<String>,
        /// Number of weeks to display (default: 52)
        #[arg(long, default_value = "52")]
        weeks: usize,
    },
}

#[derive(Subcommand)]
enum DeckAction {
    /// Create a new deck
    Create {
        #[arg(long)]
        name: String,
        /// Optional metadata as JSON
        #[arg(long)]
        meta: Option<String>,
    },
    /// List all decks
    List,
    /// Update a deck
    Update {
        /// Current deck name
        #[arg(long)]
        name: String,
        /// New name for the deck
        #[arg(long)]
        rename: Option<String>,
        /// New metadata as JSON
        #[arg(long)]
        meta: Option<String>,
        /// FSRS parameters as JSON
        #[arg(long)]
        fsrs_params: Option<String>,
    },
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

    match cli.command {
        Commands::Deck { action } => match action {
            DeckAction::Create { name, meta } => {
                let metadata: serde_json::Value = match meta {
                    Some(m) => serde_json::from_str(&m)?,
                    None => serde_json::json!({}),
                };
                let deck = tm.create_deck(&name, metadata)?;
                println!("{}", serde_json::to_string(&deck)?);
            }
            DeckAction::List => {
                let decks = tm.list_decks()?;
                println!("{}", serde_json::to_string(&decks)?);
            }
            DeckAction::Update {
                name,
                rename,
                meta,
                fsrs_params,
            } => {
                let metadata = meta.map(|m| serde_json::from_str(&m)).transpose()?;
                let params = fsrs_params.map(|p| serde_json::from_str(&p)).transpose()?;
                let deck = tm.update_deck(&name, rename.as_deref(), metadata, params)?;
                println!("{}", serde_json::to_string(&deck)?);
            }
        },

        Commands::Add { deck, content } => {
            if let Some(content_str) = content {
                let content_val: serde_json::Value = serde_json::from_str(&content_str)?;
                let id = tm.add_card(&deck, content_val)?;
                println!("{}", serde_json::json!({"id": id}));
            } else {
                let mut input = String::new();
                std::io::stdin().read_to_string(&mut input)?;
                let contents: Vec<serde_json::Value> = serde_json::from_str(&input)?;
                let ids = tm.add_cards(&deck, contents)?;
                println!("{}", serde_json::to_string(&ids)?);
            }
        }

        Commands::Import {
            deck,
            file,
            separator,
        } => {
            let text = std::fs::read_to_string(&file)?;
            let contents: Vec<serde_json::Value> = text
                .lines()
                .filter(|line| !line.trim().is_empty())
                .filter_map(|line| {
                    let parts: Vec<&str> = line.splitn(2, separator.as_str()).collect();
                    if parts.len() == 2 {
                        Some(serde_json::json!({
                            "front": parts[0].trim(),
                            "back": parts[1].trim(),
                        }))
                    } else {
                        eprintln!("Skipping malformed line: {}", line);
                        None
                    }
                })
                .collect();
            let result = tm.import_cards(&deck, contents)?;
            println!("{}", serde_json::to_string(&result)?);
        }

        Commands::Fetch { deck, limit, new } => {
            let cards = if new {
                tm.fetch_new(&deck, limit)?
            } else {
                tm.fetch_due(&deck, limit)?
            };
            println!("{}", serde_json::to_string(&cards)?);
        }

        Commands::Review => {
            let mut input = String::new();
            std::io::stdin().read_to_string(&mut input)?;
            let reviews: Vec<ReviewSubmission> = serde_json::from_str(&input)?;
            let count = reviews.len();
            tm.process_reviews(&reviews)?;
            println!(
                "{}",
                serde_json::json!({"status": "ok", "processed": count})
            );
        }

        Commands::Stats { deck } => {
            turbo_m::cli::stats::run(tm.conn(), deck.as_deref());
        }

        Commands::Tiles { deck, weeks } => {
            turbo_m::cli::tiles::run(tm.conn(), deck.as_deref(), weeks);
        }
    }

    Ok(())
}
