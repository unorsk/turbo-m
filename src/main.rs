use std::io::Read;
use std::path::PathBuf;

use clap::{Parser, Subcommand};

use turbo_m::Backend;
use turbo_m::TurboM;
use turbo_m::client::RemoteClient;
use turbo_m::models::ReviewSubmission;

#[derive(Parser)]
#[command(name = "turbo-m", about = "Spaced repetition CLI using FSRS")]
struct Cli {
    /// Path to the SQLite database file (default: ./.turbo-m.db)
    #[arg(long)]
    db: Option<PathBuf>,

    /// URL of the turbo-m remote API (e.g. https://turbo-m.example.workers.dev)
    #[arg(long, env = "TURBO_M_URL")]
    url: Option<String>,

    /// Bearer token for remote API authentication
    #[arg(long, env = "TURBO_M_TOKEN")]
    token: Option<String>,

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
    /// Interactive typing drill session
    Drill {
        /// Deck name to drill (interactive picker if omitted)
        #[arg(long)]
        deck: Option<String>,
        /// Maximum number of cards to fetch
        #[arg(long, default_value = "12")]
        limit: u32,
        /// Drill new (unreviewed) cards instead of due cards
        #[arg(long)]
        new: bool,
    },
    /// Show the hardest cards in a deck (by difficulty and lapses)
    Hardest {
        /// Deck name
        #[arg(long)]
        deck: String,
        /// Maximum number of cards to return
        #[arg(long, default_value = "12")]
        limit: u32,
        /// Output full JSON instead of plain word list
        #[arg(long)]
        json: bool,
    },
    /// Show learning statistics (all sections if no subcommand given)
    Stats {
        /// Show stats for a specific deck only
        #[arg(long)]
        deck: Option<String>,
        #[command(subcommand)]
        section: Option<StatsSection>,
    },
}

#[derive(Subcommand)]
enum StatsSection {
    /// Deck overview table
    Overview,
    /// Due forecast
    Due,
    /// Card maturity distribution
    Maturity,
    /// Review activity (last 30 days)
    Review,
    /// Rating distribution
    Dist,
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
    std::env::current_dir()
        .expect("Could not determine current directory")
        .join(".turbo-m.db")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let backend = if let Some(url) = cli.url {
        let token = cli
            .token
            .expect("--token (or TURBO_M_TOKEN env var) is required when using --url");
        Backend::Remote(RemoteClient::new(url, token))
    } else {
        let db_path = cli.db.unwrap_or_else(default_db_path);
        Backend::Local(TurboM::new(&db_path)?)
    };

    match cli.command {
        Commands::Deck { action } => match action {
            DeckAction::Create { name, meta } => {
                let metadata: serde_json::Value = match meta {
                    Some(m) => serde_json::from_str(&m)?,
                    None => serde_json::json!({}),
                };
                let deck = backend.create_deck(&name, metadata)?;
                println!("{}", serde_json::to_string(&deck)?);
            }
            DeckAction::List => {
                let decks = backend.list_decks()?;
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
                let deck = backend.update_deck(&name, rename.as_deref(), metadata, params)?;
                println!("{}", serde_json::to_string(&deck)?);
            }
        },

        Commands::Add { deck, content } => {
            if let Some(content_str) = content {
                let content_val: serde_json::Value = serde_json::from_str(&content_str)?;
                let id = backend.add_card(&deck, content_val)?;
                println!("{}", serde_json::json!({"id": id}));
            } else {
                let mut input = String::new();
                std::io::stdin().read_to_string(&mut input)?;
                let contents: Vec<serde_json::Value> = serde_json::from_str(&input)?;
                let ids = backend.add_cards(&deck, contents)?;
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
            let result = backend.import_cards(&deck, contents)?;
            println!("{}", serde_json::to_string(&result)?);
        }

        Commands::Fetch { deck, limit, new } => {
            let cards = if new {
                backend.fetch_new(&deck, limit)?
            } else {
                backend.fetch_due(&deck, limit)?
            };
            println!("{}", serde_json::to_string(&cards)?);
        }

        Commands::Review => {
            let mut input = String::new();
            std::io::stdin().read_to_string(&mut input)?;
            let reviews: Vec<ReviewSubmission> = serde_json::from_str(&input)?;
            let count = reviews.len();
            backend.process_reviews(&reviews)?;
            println!(
                "{}",
                serde_json::json!({"status": "ok", "processed": count})
            );
        }

        Commands::Hardest { deck, limit, json } => {
            let cards = backend.fetch_hardest(&deck, limit)?;
            if json {
                println!("{}", serde_json::to_string(&cards)?);
            } else {
                for card in &cards {
                    if let Some(front) = card.content.get("front").and_then(|v| v.as_str()) {
                        println!("{}", front);
                    }
                }
            }
        }

        Commands::Drill { deck, limit, new } => {
            turbo_m::cli::drill::run(&backend, deck, limit, new)?;
        }

        Commands::Stats { deck, section } => {
            let df = deck.as_deref();
            let data = backend.fetch_stats(df)?;
            match section {
                None => turbo_m::cli::stats::display_all(&data, df),
                Some(StatsSection::Overview) => turbo_m::cli::stats::print_overview(&data.overview),
                Some(StatsSection::Due) => turbo_m::cli::stats::print_forecast(&data.forecast),
                Some(StatsSection::Maturity) => turbo_m::cli::stats::print_maturity(&data.maturity),
                Some(StatsSection::Review) => turbo_m::cli::stats::print_activity(&data.activity),
                Some(StatsSection::Dist) => turbo_m::cli::stats::print_ratings(&data.ratings),
            }
        }
    }

    Ok(())
}
