use std::collections::HashMap;

use chrono::{Datelike, Duration, NaiveDate, Utc, Weekday};
use rusqlite::{Connection, params};

const BOLD: &str = "\x1b[1m";
const DIM: &str = "\x1b[2m";
const RESET: &str = "\x1b[0m";

const TILE: &str = "■ ";

// GitHub-style green palette (background → intense green)
const LEVEL_0: (u8, u8, u8) = (22, 27, 34); // #161b22  no activity
const LEVEL_1: (u8, u8, u8) = (14, 68, 41); // #0e4429
const LEVEL_2: (u8, u8, u8) = (0, 109, 50); // #006d32
const LEVEL_3: (u8, u8, u8) = (38, 166, 65); // #26a641
const LEVEL_4: (u8, u8, u8) = (57, 211, 83); // #39d353  max activity

const LEVELS: [(u8, u8, u8); 5] = [LEVEL_0, LEVEL_1, LEVEL_2, LEVEL_3, LEVEL_4];

fn rgb(r: u8, g: u8, b: u8) -> String {
    format!("\x1b[38;2;{r};{g};{b}m")
}

fn tile_color(count: usize, max: usize) -> (u8, u8, u8) {
    if count == 0 || max == 0 {
        return LEVEL_0;
    }
    if max <= 4 {
        return LEVELS[count.min(4)];
    }
    let ratio = count as f64 / max as f64;
    if ratio <= 0.25 {
        LEVEL_1
    } else if ratio <= 0.50 {
        LEVEL_2
    } else if ratio <= 0.75 {
        LEVEL_3
    } else {
        LEVEL_4
    }
}

struct DayEntry {
    date: NaiveDate,
    count: usize,
}

fn query_daily_counts(conn: &Connection, deck_filter: Option<&str>, weeks: usize) -> Vec<DayEntry> {
    let today = Utc::now().date_naive();
    let days_back = (weeks * 7) as i64;
    let start = today - Duration::days(days_back);
    let since = start.format("%Y-%m-%d").to_string();

    let deck_clause = if deck_filter.is_some() {
        "AND r.card_id IN (SELECT id FROM cards WHERE deck_id = (SELECT id FROM decks WHERE name = ?2))"
    } else {
        ""
    };

    let sql = format!(
        "SELECT date(r.review_date) as d, COUNT(DISTINCT r.review_date) as cnt
         FROM revlog r
         WHERE date(r.review_date) >= ?1
           {clause}
         GROUP BY d
         ORDER BY d ASC",
        clause = deck_clause,
    );

    let mut stmt = conn.prepare(&sql).unwrap();
    let rows: HashMap<String, usize> = if let Some(name) = deck_filter {
        stmt.query_map(params![since, name], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, i64>(1)? as usize))
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    } else {
        stmt.query_map(params![since], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, i64>(1)? as usize))
        })
        .unwrap()
        .filter_map(|r| r.ok())
        .collect()
    };

    let mut entries = Vec::new();
    let mut d = start;
    while d <= today {
        let key = d.format("%Y-%m-%d").to_string();
        let count = rows.get(&key).copied().unwrap_or(0);
        entries.push(DayEntry { date: d, count });
        d += Duration::days(1);
    }
    entries
}

fn build_weeks(entries: &[DayEntry]) -> Vec<Vec<&DayEntry>> {
    let mut weeks: Vec<Vec<&DayEntry>> = Vec::new();
    let mut current_week: Vec<&DayEntry> = Vec::new();

    for entry in entries {
        if entry.date.weekday() == Weekday::Sun && !current_week.is_empty() {
            weeks.push(current_week);
            current_week = Vec::new();
        }
        current_week.push(entry);
    }
    if !current_week.is_empty() {
        weeks.push(current_week);
    }
    weeks
}

fn transpose<'a>(weeks: &'a [Vec<&'a DayEntry>]) -> Vec<Vec<Option<&'a DayEntry>>> {
    let mut rows: Vec<Vec<Option<&'a DayEntry>>> = vec![Vec::new(); 7];

    for week in weeks {
        let mut day_slots: [Option<&DayEntry>; 7] = [None; 7];
        for entry in week {
            let dow = entry.date.weekday().num_days_from_sunday() as usize;
            day_slots[dow] = Some(entry);
        }
        for (row_idx, slot) in day_slots.iter().enumerate() {
            rows[row_idx].push(*slot);
        }
    }
    rows
}

fn print_month_header(weeks: &[Vec<&DayEntry>]) {
    let months = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];

    let mut month_line = String::new();
    let mut last_month = 0u32;

    for (col, week) in weeks.iter().enumerate() {
        let first_day = week[0];
        let month = first_day.date.month();
        if month != last_month {
            let target_pos = col * 2;
            if month_line.len() < target_pos {
                let pad = target_pos - month_line.len();
                month_line.push_str(&" ".repeat(pad));
            }
            month_line.push_str(months[(month - 1) as usize]);
            last_month = month;
        }
    }

    let day_labels_width = 4;
    println!("{}{}{}", " ".repeat(day_labels_width), month_line, RESET);
}

fn print_day_labels_and_tiles(rows: &[Vec<Option<&DayEntry>>], max_count: usize) {
    let day_labels = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

    for (row_idx, row) in rows.iter().enumerate() {
        if row_idx % 2 == 0 {
            print!("{DIM}    {RESET}");
        } else {
            print!("{DIM}{:<4}{RESET}", day_labels[row_idx]);
        }

        for slot in row {
            match slot {
                Some(entry) => {
                    let (r, g, b) = tile_color(entry.count, max_count);
                    print!("{}{TILE}{RESET}", rgb(r, g, b));
                }
                None => {
                    print!("  ");
                }
            }
        }
        println!();
    }
}

fn print_legend() {
    print!("    {DIM}Less{RESET} ");
    for &(r, g, b) in &LEVELS {
        print!("{}{TILE}{RESET}", rgb(r, g, b));
    }
    println!("{DIM}More{RESET}");
}

fn print_summary(entries: &[DayEntry]) {
    let total: usize = entries.iter().map(|e| e.count).sum();
    let active_days = entries.iter().filter(|e| e.count > 0).count();
    let total_days = entries.len();

    let mut current_streak = 0i64;
    let mut max_streak = 0i64;
    let mut streak = 0i64;

    for entry in entries.iter().rev() {
        if entry.count > 0 {
            streak += 1;
        } else {
            if current_streak == 0 && streak > 0 {
                current_streak = streak;
            }
            if streak > max_streak {
                max_streak = streak;
            }
            streak = 0;
        }
    }
    if streak > max_streak {
        max_streak = streak;
    }
    if current_streak == 0 && streak > 0 {
        current_streak = streak;
    }
    let today_entry = entries.last();
    if let Some(last) = today_entry
        && last.count == 0
    {
        current_streak = 0;
    }

    println!(
        "{BOLD}{total}{RESET} reviews in the last {total_days} days   \
         {DIM}Active days:{RESET} {active_days}   \
         {DIM}Current streak:{RESET} {current_streak}   \
         {DIM}Max streak:{RESET} {max_streak}"
    );
}

pub fn run(conn: &Connection, deck_filter: Option<&str>, weeks: usize) {
    let entries = query_daily_counts(conn, deck_filter, weeks);

    if entries.is_empty() {
        println!("{DIM}No data.{RESET}");
        return;
    }

    let max_count = entries.iter().map(|e| e.count).max().unwrap_or(0);

    println!();
    print_summary(&entries);
    println!();

    let week_groups = build_weeks(&entries);
    print_month_header(&week_groups);

    let rows = transpose(&week_groups);
    print_day_labels_and_tiles(&rows, max_count);

    print_legend();
    println!();
}
