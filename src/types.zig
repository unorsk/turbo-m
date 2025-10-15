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

/// Allows customization of matching thresholds and sensitivity
pub const MatcherConfig = struct {
    /// String length boundary that determines which threshold to use
    /// Strings longer than this value use threshold_long, shorter use threshold_short
    length_boundary: usize = 3,

    /// Maximum Levenshtein distance for short strings (length <= length_boundary)
    threshold_short: usize = 1,

    /// Maximum Levenshtein distance for long strings (length > length_boundary)
    threshold_long: usize = 2,

    /// Whether matching should be case-sensitive (not yet implemented)
    case_sensitive: bool = false,

    /// Create a default configuration with standard thresholds
    pub fn default() MatcherConfig {
        return MatcherConfig{};
    }

    /// Create a strict configuration with tighter thresholds
    pub fn strict() MatcherConfig {
        return MatcherConfig{
            .length_boundary = 5,
            .threshold_short = 0,
            .threshold_long = 1,
        };
    }

    /// Create a lenient configuration with more forgiving thresholds
    pub fn lenient() MatcherConfig {
        return MatcherConfig{
            .length_boundary = 3,
            .threshold_short = 2,
            .threshold_long = 3,
        };
    }
};
