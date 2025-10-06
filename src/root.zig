//! By convention, root.zig is the root source file when making a library.
//! This is the main library interface for the turbo-m SRS (Spaced Repetition System) application.
const std = @import("std");

pub const types = @import("types.zig");
pub const Item = types.Item;
pub const MatchType = types.MatchType;
pub const TrackedItem = types.TrackedItem;
pub const Result = types.Result;

pub const tui = @import("tui.zig");

pub const srs = struct {
    pub fn checkAnswer(expected: []const u8, actual: []const u8) MatchType {
        if (std.mem.eql(u8, expected, actual)) {
            return .exact;
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

    try testing.expectEqual(MatchType.exact, srs.checkAnswer("hello", "hello"));

    try testing.expectEqual(MatchType.incorrect, srs.checkAnswer("hello", "world"));
}
