const std = @import("std");

pub const Item = struct {
    front: []const u8,
    back: []const u8,
};

pub const MatchType = enum {
    exact,
    fuzzy,
    incorrect,
};

pub const TrackedItem = struct {
    item: Item,
    correct_count: u8,
    completed: bool,
};

pub const Result = struct {
    errors_count: u8,
    items_completed: usize,
    total_items: usize,
};
