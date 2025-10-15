//! By convention, root.zig is the root source file when making a library.
//! This is the main library interface for the turbo-m SRS (Spaced Repetition System) application.
const std = @import("std");
const levenshtein_lib = @import("levenshtein");

pub const types = @import("types.zig");
pub const Item = types.Item;
pub const MatchType = types.MatchType;
pub const TrackedItem = types.TrackedItem;
pub const Result = types.Result;
pub const MatcherConfig = types.MatcherConfig;

pub const session_controller = @import("session_controller.zig");
pub const SessionController = session_controller.SessionController;

pub const tui = @import("tui.zig");

/// Answer matching abstraction that encapsulates matching logic and configuration
/// This allows for configurable matching behavior and easy testing
pub const Matcher = struct {
    config: MatcherConfig,
    allocator: std.mem.Allocator,

    /// Create a new Matcher with the given configuration
    pub fn init(allocator: std.mem.Allocator, config: MatcherConfig) Matcher {
        return Matcher{
            .allocator = allocator,
            .config = config,
        };
    }

    /// Create a new Matcher with default configuration
    pub fn initDefault(allocator: std.mem.Allocator) Matcher {
        return init(allocator, MatcherConfig.default());
    }

    /// Match an expected answer against an actual answer
    /// Returns the match type (exact, fuzzy, or incorrect)
    pub fn match(self: *const Matcher, expected: []const u8, actual: []const u8) !MatchType {
        // Exact match before any processing
        if (std.mem.eql(u8, expected, actual)) {
            return .exact;
        }

        // Normalize strings for comparison
        const normalized_expected = try self.normalizeString(expected);
        defer self.allocator.free(normalized_expected);
        const normalized_actual = try self.normalizeString(actual);
        defer self.allocator.free(normalized_actual);

        // Check for exact match after normalization
        if (std.mem.eql(u8, normalized_expected, normalized_actual)) {
            return .exact;
        }

        // Calculate Levenshtein distance
        const distance = try levenshtein_lib.levenshtein(
            self.allocator,
            normalized_actual,
            normalized_expected,
            null,
        );

        // Apply distance thresholds based on configured length boundary
        const threshold = if (normalized_expected.len > self.config.length_boundary)
            self.config.threshold_long
        else
            self.config.threshold_short;

        if (distance <= threshold) {
            return .fuzzy;
        }

        return .incorrect;
    }

    /// Normalize a string by converting to lowercase and removing/normalizing whitespace
    /// Returns a newly allocated string that the caller must free
    fn normalizeString(self: *const Matcher, input: []const u8) ![]u8 {
        var normalized = try self.allocator.alloc(u8, input.len);
        var write_idx: usize = 0;

        for (input) |char| {
            if (std.ascii.isAlphabetic(char)) {
                normalized[write_idx] = std.ascii.toLower(char);
                write_idx += 1;
            } else if (std.ascii.isWhitespace(char)) {
                // Keep spaces but normalize them
                if (write_idx > 0 and normalized[write_idx - 1] != ' ') {
                    normalized[write_idx] = ' ';
                    write_idx += 1;
                }
            }
        }

        // Trim trailing space if any
        if (write_idx > 0 and normalized[write_idx - 1] == ' ') {
            write_idx -= 1;
        }

        return self.allocator.realloc(normalized, write_idx);
    }
};

pub const srs = struct {
    /// Convenience function for checking answers with default configuration
    /// For more control, create a Matcher instance directly
    pub fn checkAnswer(allocator: std.mem.Allocator, expected: []const u8, actual: []const u8) !MatchType {
        const matcher = Matcher.initDefault(allocator);
        return matcher.match(expected, actual);
    }

    pub fn calculateNextInterval(correct_count: u8) u32 {
        _ = correct_count;
        return 1;
    }

    /// Shuffle an array of items in place using Fisher-Yates algorithm
    pub fn shuffle(comptime T: type, items: []T, seed: u64) void {
        var prng = std.Random.DefaultPrng.init(seed);
        const rand = prng.random();
        rand.shuffle(T, items);
    }

    /// Training session that manages the state of a spaced repetition training
    pub const TrainingSession = struct {
        allocator: std.mem.Allocator,
        tracked_items: []TrackedItem,
        current_index: usize,
        completed_count: usize,
        matcher: Matcher,

        /// Initialize a training session from items with custom matcher configuration
        /// Items will be shuffled and each item needs to be answered correctly twice
        pub fn initWithMatcher(allocator: std.mem.Allocator, items: []const Item, matcher: Matcher) !TrainingSession {
            const tracked_items = try allocator.alloc(TrackedItem, items.len);

            for (items, 0..) |item, i| {
                tracked_items[i] = TrackedItem{
                    .item = item,
                    .correct_count = 0,
                    .completed = false,
                };
            }

            // Shuffle the items
            const seed = @as(u64, @intCast(std.time.nanoTimestamp()));
            shuffle(TrackedItem, tracked_items, seed);

            return TrainingSession{
                .allocator = allocator,
                .tracked_items = tracked_items,
                .current_index = 0,
                .completed_count = 0,
                .matcher = matcher,
            };
        }

        /// Initialize a training session from items with default matcher configuration
        /// Items will be shuffled and each item needs to be answered correctly twice
        pub fn init(allocator: std.mem.Allocator, items: []const Item) !TrainingSession {
            const matcher = Matcher.initDefault(allocator);
            return initWithMatcher(allocator, items, matcher);
        }

        pub fn deinit(self: *TrainingSession) void {
            self.allocator.free(self.tracked_items);
        }

        /// Get the current item to train
        /// Returns null if all items are completed
        pub fn currentItem(self: *TrainingSession) ?Item {
            if (self.isCompleted()) return null;
            return self.tracked_items[self.current_index].item;
        }

        /// Submit an answer for the current item
        /// Returns the match type and automatically advances to next item or reshuffles
        pub fn submitAnswer(self: *TrainingSession, answer: []const u8) !MatchType {
            if (self.isCompleted()) return error.SessionCompleted;

            const tracked_item = &self.tracked_items[self.current_index];
            const match_type = try self.matcher.match(tracked_item.item.back, answer);

            // Update tracking based on match type
            switch (match_type) {
                .exact, .fuzzy => {
                    tracked_item.correct_count += 1;
                    if (tracked_item.correct_count >= 2) {
                        tracked_item.completed = true;
                        self.completed_count += 1;
                    }
                },
                .incorrect => {
                    // Don't increment correct count on incorrect answers
                },
            }

            // Move to next item
            try self.advance();

            return match_type;
        }

        /// Advance to the next incomplete item, reshuffling if necessary
        fn advance(self: *TrainingSession) !void {
            // Find next incomplete item
            var found = false;
            const start_index = self.current_index + 1;

            // Search from current position to end
            for (start_index..self.tracked_items.len) |i| {
                if (!self.tracked_items[i].completed) {
                    self.current_index = i;
                    found = true;
                    break;
                }
            }

            if (!found) {
                // Need to reshuffle and start new round
                try self.reshuffleIncomplete();
            }
        }

        /// Reshuffle incomplete items and start a new round
        fn reshuffleIncomplete(self: *TrainingSession) !void {
            // Count incomplete items
            var incomplete_count: usize = 0;
            for (self.tracked_items) |tracked_item| {
                if (!tracked_item.completed) {
                    incomplete_count += 1;
                }
            }

            if (incomplete_count == 0) {
                return;
            }

            // Collect incomplete items
            var incomplete_items = try self.allocator.alloc(TrackedItem, incomplete_count);
            defer self.allocator.free(incomplete_items);

            var idx: usize = 0;
            for (self.tracked_items) |tracked_item| {
                if (!tracked_item.completed) {
                    incomplete_items[idx] = tracked_item;
                    idx += 1;
                }
            }

            // Shuffle incomplete items
            const seed = @as(u64, @intCast(std.time.nanoTimestamp()));
            shuffle(TrackedItem, incomplete_items, seed);

            // Place shuffled incomplete items at the beginning
            var write_idx: usize = 0;
            for (incomplete_items) |item| {
                self.tracked_items[write_idx] = item;
                write_idx += 1;
            }

            // Place completed items after
            for (self.tracked_items) |tracked_item| {
                if (tracked_item.completed) {
                    self.tracked_items[write_idx] = tracked_item;
                    write_idx += 1;
                }
            }

            // Reset to beginning
            self.current_index = 0;
        }

        /// Check if the training session is completed
        pub fn isCompleted(self: *TrainingSession) bool {
            return self.completed_count >= self.tracked_items.len;
        }

        /// Get total number of items
        pub fn totalItems(self: *TrainingSession) usize {
            return self.tracked_items.len;
        }

        /// Get number of completed items
        pub fn completedItems(self: *TrainingSession) usize {
            return self.completed_count;
        }
    };
};

test "basic functionality - convenience function" {
    const testing = std.testing;

    try testing.expectEqual(MatchType.exact, try srs.checkAnswer(testing.allocator, "hello", "hello"));

    try testing.expectEqual(MatchType.incorrect, try srs.checkAnswer(testing.allocator, "hello", "world"));

    // Test fuzzy matching
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "hello", "helo"));
    try testing.expectEqual(MatchType.exact, try srs.checkAnswer(testing.allocator, "hello", "Hello")); // Case normalized to exact
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "test", "tst"));
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "cat", "ca"));
}

test "Matcher - default configuration" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    // Exact matches
    try testing.expectEqual(MatchType.exact, try matcher.match("hello", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("test", "test"));

    // Case-insensitive exact matches
    try testing.expectEqual(MatchType.exact, try matcher.match("Hello", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("HELLO", "hello"));

    // Whitespace normalization
    try testing.expectEqual(MatchType.exact, try matcher.match("hello  world", "hello world"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello\tworld", "hello world"));

    // Short strings (length <= 3): threshold = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("cat", "ca"));
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("dog", "do"));
    try testing.expectEqual(MatchType.incorrect, try matcher.match("cat", "c")); // distance = 2

    // Long strings (length > 3): threshold = 2
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "helo")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "hllo")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "tesing")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "helo")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "heo")); // distance = 2 (at boundary, still fuzzy)

    // Completely different strings
    try testing.expectEqual(MatchType.incorrect, try matcher.match("hello", "world"));
    try testing.expectEqual(MatchType.incorrect, try matcher.match("cat", "dog"));
}

test "Matcher - strict configuration" {
    const testing = std.testing;
    const config = MatcherConfig.strict();
    const matcher = Matcher.init(testing.allocator, config);

    // Exact matches still work
    try testing.expectEqual(MatchType.exact, try matcher.match("hello", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("Hello", "hello"));

    // Short strings (length <= 5): threshold = 0 (only exact matches)
    try testing.expectEqual(MatchType.incorrect, try matcher.match("cat", "ca"));
    try testing.expectEqual(MatchType.incorrect, try matcher.match("hello", "helo"));

    // Long strings (length > 5): threshold = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "tesing")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "tsting")); // distance = 1 (one insertion)
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "testng")); // distance = 1 (one deletion)
    try testing.expectEqual(MatchType.incorrect, try matcher.match("testing", "tting")); // distance = 2 (two deletions)
}

test "Matcher - lenient configuration" {
    const testing = std.testing;
    const config = MatcherConfig.lenient();
    const matcher = Matcher.init(testing.allocator, config);

    // Exact matches still work
    try testing.expectEqual(MatchType.exact, try matcher.match("hello", "hello"));

    // Short strings (length <= 3): threshold = 2
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("cat", "ca")); // distance = 1
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("cat", "c")); // distance = 2
    try testing.expectEqual(MatchType.incorrect, try matcher.match("cat", "x")); // distance = 3

    // Long strings (length > 3): threshold = 3
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "heo")); // distance = 2
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "teing")); // distance = 2
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("example", "exmpl")); // distance = 2
}

test "Matcher - custom configuration" {
    const testing = std.testing;
    const config = MatcherConfig{
        .length_boundary = 5,
        .threshold_short = 2,
        .threshold_long = 4,
    };
    const matcher = Matcher.init(testing.allocator, config);

    // Short strings (length <= 5): threshold = 2
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("hello", "heo")); // distance = 2
    try testing.expectEqual(MatchType.incorrect, try matcher.match("hello", "ho")); // distance = 3

    // Long strings (length > 5): threshold = 4
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("testing", "tst")); // distance = 3
    try testing.expectEqual(MatchType.fuzzy, try matcher.match("example", "expl")); // distance = 2
}

test "Matcher - edge cases: empty strings" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    // Empty string matches empty string
    try testing.expectEqual(MatchType.exact, try matcher.match("", ""));

    // Non-empty vs empty string
    try testing.expectEqual(MatchType.incorrect, try matcher.match("hello", ""));
    try testing.expectEqual(MatchType.incorrect, try matcher.match("", "hello"));

    // Single character
    try testing.expectEqual(MatchType.exact, try matcher.match("a", "a"));
    try testing.expectEqual(MatchType.exact, try matcher.match("A", "a"));
}

test "Matcher - edge cases: special characters" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    // Special characters are normalized out
    try testing.expectEqual(MatchType.exact, try matcher.match("hello!", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello?", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello-world", "helloworld"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello_world", "helloworld"));

    // Numbers are normalized out (only alphabetic characters kept)
    try testing.expectEqual(MatchType.exact, try matcher.match("test123", "test"));
    try testing.expectEqual(MatchType.exact, try matcher.match("123test", "test"));
}

test "Matcher - edge cases: long strings" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    const long_expected = "this is a very long string that tests the matcher behavior with longer inputs";
    const long_actual_exact = "this is a very long string that tests the matcher behavior with longer inputs";
    const long_actual_fuzzy = "this is a very long string that tests the matcher behavor with longer inputs"; // 'i' missing
    const long_actual_wrong = "completely different long string with nothing in common whatsoever";

    try testing.expectEqual(MatchType.exact, try matcher.match(long_expected, long_actual_exact));
    try testing.expectEqual(MatchType.fuzzy, try matcher.match(long_expected, long_actual_fuzzy));
    try testing.expectEqual(MatchType.incorrect, try matcher.match(long_expected, long_actual_wrong));
}

test "Matcher - edge cases: multiple spaces and tabs" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    // Multiple spaces collapsed to single space
    try testing.expectEqual(MatchType.exact, try matcher.match("hello    world", "hello world"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello\t\tworld", "hello world"));
    try testing.expectEqual(MatchType.exact, try matcher.match("  hello  world  ", "hello world"));

    // Leading and trailing whitespace handled
    try testing.expectEqual(MatchType.exact, try matcher.match("   hello", "hello"));
    try testing.expectEqual(MatchType.exact, try matcher.match("hello   ", "hello"));
}

test "Matcher - normalization isolates matching logic" {
    const testing = std.testing;
    const matcher = Matcher.initDefault(testing.allocator);

    // These should all be considered exact matches after normalization
    try testing.expectEqual(MatchType.exact, try matcher.match("Hello World!", "hello world"));
    try testing.expectEqual(MatchType.exact, try matcher.match("HELLO-WORLD", "helloworld"));
    try testing.expectEqual(MatchType.exact, try matcher.match("  HeLLo   WoRLd  ", "hello world"));
}
