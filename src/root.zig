//! By convention, root.zig is the root source file when making a library.
//! This is the main library interface for the turbo-m SRS (Spaced Repetition System) application.
const std = @import("std");
const levenshtein_lib = @import("levenshtein");

pub const types = @import("types.zig");
pub const Item = types.Item;
pub const MatchType = types.MatchType;
pub const TrackedItem = types.TrackedItem;
pub const Result = types.Result;

pub const tui = @import("tui.zig");

fn normalizeString(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var normalized = try allocator.alloc(u8, input.len);
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

    return allocator.realloc(normalized, write_idx);
}

pub const srs = struct {
    pub fn checkAnswer(allocator: std.mem.Allocator, expected: []const u8, actual: []const u8) !MatchType {
        // Exact match
        if (std.mem.eql(u8, expected, actual)) {
            return .exact;
        }

        // Normalize strings for comparison
        const normalized_expected = try normalizeString(allocator, expected);
        defer allocator.free(normalized_expected);
        const normalized_actual = try normalizeString(allocator, actual);
        defer allocator.free(normalized_actual);

        // Check for exact match after normalization
        if (std.mem.eql(u8, normalized_expected, normalized_actual)) {
            return .exact;
        }

        // Calculate Levenshtein distance
        const distance = try levenshtein_lib.levenshtein(allocator, normalized_actual, normalized_expected, null);

        // Apply distance thresholds based on length
        const threshold = if (normalized_expected.len > 3) @as(usize, 2) else @as(usize, 1);

        if (distance <= threshold) {
            return .fuzzy;
        }

        return .incorrect;
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

        /// Initialize a training session from items
        /// Items will be shuffled and each item needs to be answered correctly twice
        pub fn init(allocator: std.mem.Allocator, items: []const Item) !TrainingSession {
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
            };
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
            const match_type = try checkAnswer(self.allocator, tracked_item.item.back, answer);

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
            self.advance();

            return match_type;
        }

        /// Advance to the next incomplete item, reshuffling if necessary
        fn advance(self: *TrainingSession) void {
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
                self.reshuffleIncomplete();
            }
        }

        /// Reshuffle incomplete items and start a new round
        fn reshuffleIncomplete(self: *TrainingSession) void {
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
            var incomplete_items = self.allocator.alloc(TrackedItem, incomplete_count) catch return;
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

test "basic functionality" {
    const testing = std.testing;

    try testing.expectEqual(MatchType.exact, try srs.checkAnswer(testing.allocator, "hello", "hello"));

    try testing.expectEqual(MatchType.incorrect, try srs.checkAnswer(testing.allocator, "hello", "world"));

    // Test fuzzy matching
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "hello", "helo"));
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "hello", "Hello"));
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "test", "tst"));
    try testing.expectEqual(MatchType.fuzzy, try srs.checkAnswer(testing.allocator, "cat", "ca"));
}
