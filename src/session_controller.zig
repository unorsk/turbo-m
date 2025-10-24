const std = @import("std");
const types = @import("types.zig");
const root = @import("root.zig");
const builtin = @import("builtin");

fn getVoiceForLanguage(lang: []const u8) ?[]const u8 {
    _ = lang;
    return "Anna";
}

fn say(message: []const u8, alloc: std.mem.Allocator) !void {
    const language = "de";
    if (builtin.target.os.tag.isDarwin()) {
        if (getVoiceForLanguage(language)) |voice| {
            _ = try std.process.Child.run(.{
                .allocator = alloc,
                .argv = &[_][]const u8{ "say", "-v", voice, message },
            });
        }
    }
}

/// SessionController manages the state and business logic of a training session
/// This is separated from UI concerns to make the logic testable and reusable
pub const SessionController = struct {
    allocator: std.mem.Allocator,
    session: root.srs.TrainingSession,
    errors_count: u8,
    current_state: State,

    /// Represents all possible states in the training session flow
    pub const State = union(enum) {
        /// Showing a question to the user
        showing_question: struct {
            item: types.Item,
            progress: Progress,
        },
        /// Showing feedback after user submitted an answer
        showing_feedback: struct {
            item: types.Item,
            match_type: types.MatchType,
            user_answer: []const u8,
            progress: Progress,
        },
        /// Training session completed
        completed: struct {
            errors_count: u8,
            items_completed: usize,
            total_items: usize,
        },
    };

    pub const Progress = struct {
        completed: usize,
        total: usize,
    };

    /// Initialize a new SessionController with the given items
    pub fn init(allocator: std.mem.Allocator, items: []const types.Item) !SessionController {
        var session = try root.srs.TrainingSession.init(allocator, items);

        const initial_state = if (session.currentItem()) |item|
            State{
                .showing_question = .{
                    .item = item,
                    .progress = .{
                        .completed = session.completedItems(),
                        .total = session.totalItems(),
                    },
                },
            }
        else
            State{
                .completed = .{
                    .errors_count = 0,
                    .items_completed = session.completedItems(),
                    .total_items = session.totalItems(),
                },
            };

        return SessionController{
            .allocator = allocator,
            .session = session,
            .errors_count = 0,
            .current_state = initial_state,
        };
    }

    pub fn deinit(self: *SessionController) void {
        self.session.deinit();
    }

    /// Get the current state of the session
    pub fn getState(self: *const SessionController) State {
        return self.current_state;
    }

    /// Submit an answer for the current question
    /// This advances the state machine and returns the new state
    pub fn submitAnswer(self: *SessionController, answer: []const u8) !State {
        // Can only submit answer when showing a question
        switch (self.current_state) {
            .showing_question => |question_state| {
                // Submit answer to training session
                const match_type = try self.session.submitAnswer(answer);

                // Track errors
                if (match_type != .exact) {
                    self.errors_count += 1;
                }

                // Transition to showing feedback if not exact match
                if (match_type != .exact) {
                    const tracked_item = &self.session.tracked_items[self.session.current_index - 1];
                    try say(tracked_item.item.back, self.allocator);
                    // Need to allocate a copy of the answer for the feedback state
                    const answer_copy = try self.allocator.dupe(u8, answer);
                    self.current_state = State{
                        .showing_feedback = .{
                            .item = question_state.item,
                            .match_type = match_type,
                            .user_answer = answer_copy,
                            .progress = .{
                                .completed = self.session.completedItems(),
                                .total = self.session.totalItems(),
                            },
                        },
                    };
                } else {
                    // Exact match - advance to next question or completion
                    try self.advanceToNextQuestion();
                }

                return self.current_state;
            },
            .showing_feedback => return error.InvalidState,
            .completed => return error.SessionCompleted,
        }
    }

    /// Continue from feedback state to next question
    pub fn continueFeedback(self: *SessionController) !State {
        switch (self.current_state) {
            .showing_feedback => |feedback_state| {
                // Free the user answer copy
                self.allocator.free(feedback_state.user_answer);

                // Advance to next question
                try self.advanceToNextQuestion();
                return self.current_state;
            },
            .showing_question => return error.InvalidState,
            .completed => return error.SessionCompleted,
        }
    }

    /// Internal: advance to the next question or mark as completed
    fn advanceToNextQuestion(self: *SessionController) !void {
        if (self.session.currentItem()) |item| {
            self.current_state = State{
                .showing_question = .{
                    .item = item,
                    .progress = .{
                        .completed = self.session.completedItems(),
                        .total = self.session.totalItems(),
                    },
                },
            };
        } else {
            self.current_state = State{
                .completed = .{
                    .errors_count = self.errors_count,
                    .items_completed = self.session.completedItems(),
                    .total_items = self.session.totalItems(),
                },
            };
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "SessionController - initialization with items" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
        .{ .front = "foo", .back = "bar" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    const state = controller.getState();
    try testing.expect(state == .showing_question);
    try testing.expectEqual(@as(usize, 2), state.showing_question.progress.total);
    try testing.expectEqual(@as(usize, 0), state.showing_question.progress.completed);
}

test "SessionController - initialization with empty items" {
    const testing = std.testing;

    const items = [_]types.Item{};

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    const state = controller.getState();
    try testing.expect(state == .completed);
    try testing.expectEqual(@as(usize, 0), state.completed.total_items);
}

test "SessionController - exact answer advances to next question" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
        .{ .front = "foo", .back = "bar" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Initial state should be showing first question
    var state = controller.getState();
    try testing.expect(state == .showing_question);
    const first_item = state.showing_question.item;

    // Submit exact answer
    state = try controller.submitAnswer(first_item.back);

    // Should advance to next question (or same question since we need 2 correct answers)
    try testing.expect(state == .showing_question);
    try testing.expectEqual(@as(u8, 0), controller.errors_count);
}

test "SessionController - incorrect answer shows feedback" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Submit incorrect answer
    const state = try controller.submitAnswer("wrong");

    // Should show feedback
    try testing.expect(state == .showing_feedback);
    try testing.expectEqual(types.MatchType.incorrect, state.showing_feedback.match_type);
    try testing.expectEqualStrings("wrong", state.showing_feedback.user_answer);
    try testing.expectEqualStrings("world", state.showing_feedback.item.back);
    try testing.expectEqual(@as(u8, 1), controller.errors_count);
}

test "SessionController - fuzzy answer shows feedback" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Submit fuzzy answer (one character off)
    const state = try controller.submitAnswer("wrld");

    // Should show feedback
    try testing.expect(state == .showing_feedback);
    try testing.expectEqual(types.MatchType.fuzzy, state.showing_feedback.match_type);
    try testing.expectEqualStrings("wrld", state.showing_feedback.user_answer);
    try testing.expectEqual(@as(u8, 1), controller.errors_count);
}

test "SessionController - continue from feedback advances to next question" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Submit incorrect answer to get to feedback state
    _ = try controller.submitAnswer("wrong");

    // Continue from feedback
    const state = try controller.continueFeedback();

    // Should be back to showing a question
    try testing.expect(state == .showing_question);
}

test "SessionController - completing all items transitions to completed state" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Answer correctly twice (required by training session)
    var i: usize = 0;
    while (i < 100) : (i += 1) { // Safety limit
        const state = controller.getState();
        if (state == .completed) break;

        switch (state) {
            .showing_question => |q| {
                _ = try controller.submitAnswer(q.item.back);
            },
            .showing_feedback => {
                _ = try controller.continueFeedback();
            },
            .completed => break,
        }
    }

    const final_state = controller.getState();
    try testing.expect(final_state == .completed);
    try testing.expectEqual(@as(usize, 1), final_state.completed.items_completed);
    try testing.expectEqual(@as(usize, 1), final_state.completed.total_items);
}

test "SessionController - cannot submit answer when in feedback state" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Get to feedback state
    _ = try controller.submitAnswer("wrong");

    // Try to submit another answer (should error)
    const result = controller.submitAnswer("another");
    try testing.expectError(error.InvalidState, result);
}

test "SessionController - cannot continue feedback when in question state" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "hello", .back = "world" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Try to continue feedback when in question state (should error)
    const result = controller.continueFeedback();
    try testing.expectError(error.InvalidState, result);
}

test "SessionController - tracks errors across multiple items" {
    const testing = std.testing;

    const items = [_]types.Item{
        .{ .front = "q1", .back = "a1" },
        .{ .front = "q2", .back = "a2" },
    };

    var controller = try SessionController.init(testing.allocator, &items);
    defer controller.deinit();

    // Make several errors
    _ = try controller.submitAnswer("wrong1");
    try testing.expectEqual(@as(u8, 1), controller.errors_count);

    _ = try controller.continueFeedback();

    _ = try controller.submitAnswer("wrong2");
    try testing.expectEqual(@as(u8, 2), controller.errors_count);

    _ = try controller.continueFeedback();

    _ = try controller.submitAnswer("wrong3");
    try testing.expectEqual(@as(u8, 3), controller.errors_count);
}
