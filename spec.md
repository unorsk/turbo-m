I want to do a spaced repetition app using fsrs so I could learn words more effectively.


Right now I have small decks, like 12-14 words each and every week I start with a new deck.

The words get shuffled and I have to type every word correctly three times.


This works very good for me because the easiest words I type three times, but the most difficult words take more attempts.


But the system has no memory about which words are easy/difficult and when I refreshed them.


This is why I want to start using a fsrs algorithm.


My idea is that the app would have two modes:

1. Warm up, this is when I say: "okay, I'll learn some new words", and it would take the top 12-14 fresh words from the deck, then it will take me through the whole three loop as before (but only save results from my first attempt to guess the words to make the scheduling)

2. Refresh - this would take 12-14 words that need refreshing, and then it will also do the loops, where I have to enter every word correctly at least three times, but it would only count as correct/wrong the first attempt.



What I also think is that in this system, the retention would be measured as good/bad/okay, where as 'okay' would count attempts that were almost correct - the Levenshtein distance has to be less or equal than two, but not zero.


This is how the cli would work. What I also want to make is a web interface that would work the following mode: Reverse + choice.


This would work like this: It will fetch 12-14 words that are the hardest according to the system (or maybe the ones that have to be refreshed soon or the ones I've recently refreshed, I am not sure and need suggestions here) it would read the word that I had to remember (and also display it) and then it will present with four translation options where I have to pick one. In this mode it wouldn't update any srs retention/refresh things, it's just bonus training.



System Specification: FSRS Drill-CLI & Web Companion1. System OverviewThis system is a hybrid Spaced Repetition System (SRS) that combines long-term scheduling (FSRS algorithm) with high-repetition short-term drills. It solves the "forgetting loop" problem by enforcing a "correct 3 times" rule during every practice session, while ensuring only the initial attempt impacts the long-term schedule.Core Philosophy:FSRS (Scheduler): Determines what to study and when. It only cares about your raw, unassisted performance (First Attempt).Drill Loop (Gym): Ensures you don't leave the session until you have mastered the word for the day. It cares about repetition and muscle memory.2. Technology StackLanguage: Rust (Edition 2021+)CLI Interface: rustline (for robust input handling, history, and editing)Web Backend: actix-web (REST API for the web interface)Algorithm: fsrs-rs (Official/Community Rust crate for FSRS v5)Comparison: levenshtein crate (for fuzzy string matching)Storage: JSON file-based storage (for simplicity and portability), serialized via serde_json.3. Data Structures3.1 The Card ModelThis is the single source of truth for a word.Ruststruct Card {
    id: String,             // UUID
    front: String,          // Native Language (L1) - e.g., "Apple"
    back: String,           // Target Language (L2) - e.g., "Pomme"
    
    // FSRS State
    state: FSRSState,       // New, Learning, Review, Relearning
    stability: f64,
    difficulty: f64,
    elapsed_days: u64,
    scheduled_days: u64,
    due_date: DateTime<Utc>,
    last_review: Option<DateTime<Utc>>,

    // Metrics
    lapses: u32,            // Total times graded 'Again'
}
3.2 The Runtime Session StructThis tracks the temporary state during a live CLI session. It is not saved to disk.Ruststruct SessionItem {
    card: Card,
    success_counter: u8,    // Starts at 0, must reach 3 to exit
    is_graded: bool,        // False initially. Becomes True after 1st attempt.
}
4. Grading Algorithm (The "Brain")The system must map user input to FSRS ratings using Levenshtein distance.Definitions:Input: User's typed string (trimmed, lowercase normalized).Target: The correct back of the card (trimmed, lowercase normalized).Distance: Levenshtein distance between Input and Target.Len: Length of the Target string.The Logic Table:ConditionGradeFSRS ActionDrill ActionDistance == 0GoodUpdate Stability/Diffsuccess_counter += 1Distance <= 1 (if Len < 5)HardUpdate Stability/Diffsuccess_counter remains 0 (Strict Mode)Distance <= 2 (if Len >= 5)HardUpdate Stability/Diffsuccess_counter remains 0 (Strict Mode)Distance > ThresholdAgainReset Stabilitysuccess_counter = 0Strict Mode Rule: If a user makes a typo (Hard), they have not "mastered" the typing. We record the progress as "Hard" for the algorithm, but strictly require them to restart the "3x correct" loop to fix the spelling error.5. CLI SpecificationThe CLI is the primary "Write" interface where learning happens. It has two modes.Mode A: Warm Up (New Words)Trigger: User selects "Warm Up" from menu.Scope: Fetches N (default 12) cards where state == New.Workflow:Selection: Pull 12 distinct New cards.Queue Initialization: Convert cards into SessionItems with success_counter = 0.The Loop:Pick: Select a card from the queue. (Priority: Card with lowest success count, avoiding the immediately preceding card if possible).Display: Show Front (L1).Input: User types Back (L2) using rustline.Logic:If First Attempt (!is_graded):Calculate Grade (Again/Hard/Good).CRITICAL: Apply fsrs.next(card, grade, now) immediately and update the internal Card struct.Set is_graded = true.If Repeat Attempt (is_graded):Ignore FSRS. Only check correctness for the Drill Loop.Feedback:Show "Correct" (Green) or "Wrong: [Target]" (Red).Show "Repetitions left: X" (e.g., 2/3).Queue Management:If success_counter == 3: Remove item from queue.Else: Return item to queue.Completion: When queue is empty, save the updated FSRS state to cards.json.Mode B: Refresh (Review)Trigger: User selects "Refresh" from menu.Scope: Fetches N cards where due_date <= Now, sorted by retrievability (ascending).Workflow:Selection: Identify all Due cards. Sort by lowest retrievability. Take top 12.Queue Initialization: Same as Warm Up.The Loop: Identical to Warm Up.Note: Even if the user remembers the word perfectly (Grade: Good), they must still type it 3 times (Drill Loop). This reinforces the memory immediately.Optional Optimization: If Grade is Easy (Perfect match + Time < 2s), you may set success_counter = 3 immediately to skip the drill for trivial words.6. Web Interface Specification (Actix)The Web Interface is the "Read-Only" (mostly) bonus trainer. It does not update FSRS scheduling to prevent data corruption from passive recognition.Endpoint 1: GET /api/bonus-trainingLogic: Returns a JSON payload of 12-14 "Challenge Cards".Selection Priority (The "Triage" Strategy):Lapses: Cards graded Again in the last 24 hours.Leeches: Cards with difficulty > 7.Impending: Cards due within the next 24 hours.Response Schema:JSON[
  {
    "card_id": "uuid",
    "target_word": "Pomme", // Display this (L2)
    "options": [
      "Apple",   // Correct (L1)
      "Pear",    // Distractor
      "Banana",  // Distractor
      "Grape"    // Distractor
    ]
  }
]
Distractor Logic:Select 3 random Front values from the same Deck.Ideally, filter for words of similar length to the correct answer to prevent guessing by size.Endpoint 2: POST /api/bonus-submitLogic: Receives the user's choice.Action:Does NOT update FSRS interval.MAY update a separate "streak" counter or "bonus xp" for gamification.Returns: { "correct": boolean, "correct_answer": "Apple" }7. Development Implementation StepsProject Setup:cargo new srs_drillAdd dependencies: serde, serde_json, chrono, fsrs, actix-web, rustline, uuid, strsim (for Levenshtein).Core Library (lib.rs):Implement Card struct and JSON Load/Save traits.Implement FsrsHandler struct that wraps the fsrs crate logic.CLI (bin/cli.rs):Implement the Session loop.Implement the Levenshtein grading logic.Connect rustline for input.Web Server (bin/server.rs):Spin up Actix.Load the same JSON file (ensure Read-Only access or simple mutex locking if running simultaneously).Implement the "Reverse + Choice" generator.8. Summary of Logic Flow (Pseudo-Code)Rustfn handle_input(input: &str, session_item: &mut SessionItem) {
    let dist = levenshtein(input, &session_item.card.back);
    
    // 1. FSRS LOGIC (First Attempt Only)
    if !session_item.is_graded {
        let rating = match dist {
            0 => Rating::Good,
            d if d <= 2 => Rating::Hard,
            _ => Rating::Again
        };
        
        // Update the card's permanent schedule
        update_fsrs_schedule(&mut session_item.card, rating);
        session_item.is_graded = true;
    }

    // 2. DRILL LOGIC (Always)
    if dist == 0 {
        session_item.success_counter += 1;
        println!("Correct! ({}/3)", session_item.success_counter);
    } else {
        // Reset counter on ANY error to force mastery
        session_item.success_counter = 0; 
        println!("Wrong. Correct is: {}", session_item.card.back);
    }
}