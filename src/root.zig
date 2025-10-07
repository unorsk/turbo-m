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
