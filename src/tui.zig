const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const types = @import("types.zig");
const root = @import("root.zig");

const log = std.log.scoped(.tui);

const Model = struct {
    alloc: std.mem.Allocator,
    controller: root.SessionController,
    text_field: vxfw.TextField,
    ok: vxfw.Button,
    cancel: vxfw.Button,

    fn onClick(_: ?*anyopaque, ctx: *vxfw.EventContext) anyerror!void {
        // const ptr = maybe_ptr orelse return;
        // const self: *Model = @ptrCast(@alignCast(ptr));
        // self.count +|= 1;
        return ctx.consumeAndRedraw();
    }

    pub fn widget(self: *Model) vxfw.Widget {
        return .{
            .userdata = self,
            .eventHandler = Model.typeErasedEventHandler,
            .drawFn = Model.typeErasedDrawFn,
        };
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        switch (event) {
            .init => return ctx.requestFocus(self.text_field.widget()),
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    ctx.quit = true;
                    return;
                }

                const current_state = self.controller.getState();

                // Handle state-specific key presses
                switch (current_state) {
                    .showing_question => {
                        if (key.matches(vaxis.Key.enter, .{})) {
                            const user_input = try self.text_field.buf.dupe();
                            defer self.alloc.free(user_input);

                            // Submit answer through controller
                            const new_state = self.controller.submitAnswer(user_input) catch |err| {
                                switch (err) {
                                    error.OutOfMemory => {
                                        log.err("Out of memory during session operation", .{});
                                        ctx.quit = true;
                                        return error.OutOfMemory;
                                    },
                                    error.SessionCompleted => {
                                        ctx.quit = true;
                                        return;
                                    },
                                    else => return err,
                                }
                            };

                            // Clear input field on exact match
                            if (new_state == .showing_question) {
                                self.text_field.buf.clearRetainingCapacity();
                            }

                            // Quit if completed
                            if (new_state == .completed) {
                                ctx.quit = true;
                                return;
                            }

                            return ctx.consumeAndRedraw();
                        }
                    },
                    .showing_feedback => {
                        // Any key continues from feedback
                        const new_state = self.controller.continueFeedback() catch |err| {
                            switch (err) {
                                error.InvalidState, error.SessionCompleted => {
                                    ctx.quit = true;
                                    return;
                                },
                            }
                        };

                        // Clear input field
                        self.text_field.buf.clearRetainingCapacity();

                        // Quit if completed
                        if (new_state == .completed) {
                            ctx.quit = true;
                            return;
                        }

                        return ctx.consumeAndRedraw();
                    },
                    .completed => {
                        ctx.quit = true;
                        return;
                    },
                }
            },
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        const max_size = ctx.max.size();

        const current_state = self.controller.getState();

        // Render based on current state
        switch (current_state) {
            .showing_question => |question_state| {
                return try self.drawQuestionView(ctx, max_size, question_state.item);
            },
            .showing_feedback => |feedback_state| {
                return try self.drawFeedbackView(ctx, max_size, feedback_state);
            },
            .completed => {
                return try self.drawCompletionView(ctx, max_size);
            },
        }
    }

    fn drawQuestionView(self: *Model, ctx: vxfw.DrawContext, max_size: vxfw.Size, item: types.Item) !vxfw.Surface {
        const front_text: vxfw.Text = .{ .text = item.front };
        const front_surface = try front_text.draw(ctx);

        const front_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2) - 3,
                .col = @divTrunc(max_size.width, 2) - @divTrunc(front_surface.size.width, 2),
            },
            .surface = front_surface,
        };

        const border: vxfw.Border = .{
            .child = self.text_field.widget(),
        };
        const border_surface = try border.draw(ctx.withConstraints(
            ctx.min,
            .{ .width = 62, .height = 3 },
        ));

        const border_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2) + 2,
                .col = @divTrunc(max_size.width, 2) - 31,
            },
            .surface = border_surface,
        };

        const ok: vxfw.SubSurface = .{
            .origin = .{ .row = @divTrunc(max_size.height, 2) + 5, .col = @divTrunc(max_size.width, 2) + 31 - 17 },
            .surface = try self.ok.draw(ctx.withConstraints(
                ctx.min,
                .{ .width = 8, .height = 1 },
            )),
        };

        const cancel: vxfw.SubSurface = .{
            .origin = .{ .row = @divTrunc(max_size.height, 2) + 5, .col = @divTrunc(max_size.width, 2) + 31 - 8 },
            .surface = try self.cancel.draw(ctx.withConstraints(
                ctx.min,
                .{ .width = 8, .height = 1 },
            )),
        };

        const children = try ctx.arena.alloc(vxfw.SubSurface, 4);
        children[0] = front_child;
        children[1] = border_child;
        children[2] = ok;
        children[3] = cancel;

        return .{
            .size = max_size,
            .widget = self.widget(),
            .buffer = &.{},
            .children = children,
        };
    }

    fn drawFeedbackView(self: *Model, ctx: vxfw.DrawContext, max_size: vxfw.Size, feedback: anytype) !vxfw.Surface {
        const status_text_str = if (feedback.match_type == .fuzzy) "≈" else "⚠️";
        const correct_text_str = try std.fmt.allocPrint(ctx.arena, "{s} Correct answer: {s}", .{ status_text_str, feedback.item.back });
        const correct_text: vxfw.Text = .{ .text = correct_text_str };
        const correct_surface = try correct_text.draw(ctx);

        const correct_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2) - 3,
                .col = @divTrunc(max_size.width, 2) - @divTrunc(correct_surface.size.width, 2),
            },
            .surface = correct_surface,
        };

        const continue_text: vxfw.Text = .{ .text = "Press any key to continue..." };
        const continue_surface = try continue_text.draw(ctx);

        const continue_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2) + 8,
                .col = @divTrunc(max_size.width, 2) - @divTrunc(continue_surface.size.width, 2),
            },
            .surface = continue_surface,
        };

        // Include text field in surface tree to maintain focus
        const border: vxfw.Border = .{
            .child = self.text_field.widget(),
        };
        const border_surface = try border.draw(ctx.withConstraints(
            ctx.min,
            .{ .width = 62, .height = 3 },
        ));

        const text_field_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2) + 2,
                .col = @divTrunc(max_size.width, 2) - 31,
            },
            .surface = border_surface,
        };

        const children = try ctx.arena.alloc(vxfw.SubSurface, 3);
        children[0] = correct_child;
        children[1] = continue_child;
        children[2] = text_field_child;

        return .{
            .size = max_size,
            .widget = self.widget(),
            .buffer = &.{},
            .children = children,
        };
    }

    fn drawCompletionView(self: *Model, ctx: vxfw.DrawContext, max_size: vxfw.Size) !vxfw.Surface {
        const completion_text: vxfw.Text = .{ .text = "All items completed!" };
        const text_surface = try completion_text.draw(ctx);

        const text_child: vxfw.SubSurface = .{
            .origin = .{
                .row = @divTrunc(max_size.height, 2),
                .col = @divTrunc(max_size.width, 2) - @divTrunc(text_surface.size.width, 2),
            },
            .surface = text_surface,
        };

        const children = try ctx.arena.alloc(vxfw.SubSurface, 1);
        children[0] = text_child;

        return .{
            .size = max_size,
            .widget = self.widget(),
            .buffer = &.{},
            .children = children,
        };
    }
};

pub fn run(alloc: std.mem.Allocator, items: []const types.Item) !types.Result {
    var app = try vxfw.App.init(alloc);
    defer app.deinit();

    // Initialize session controller
    var controller = try root.SessionController.init(alloc, items);
    defer controller.deinit();

    const model = try alloc.create(Model);
    defer alloc.destroy(model);

    model.* = .{
        .alloc = alloc,
        .controller = controller,
        .text_field = vxfw.TextField.init(alloc, &app.vx.unicode),
        .ok = .{
            .label = "Easy",
            .onClick = Model.onClick,
            .userdata = model,
        },
        .cancel = .{ .label = "Hard", .onClick = Model.onClick, .userdata = model, .style = .{ .default = .{ .bg = .{ .rgb = .{ 255, 0, 0 } } } } },
    };
    defer model.text_field.deinit();

    try app.run(model.widget(), .{});

    // Get final state from controller
    const final_state = controller.getState();
    return switch (final_state) {
        .completed => |c| types.Result{
            .errors_count = c.errors_count,
            .items_completed = c.items_completed,
            .total_items = c.total_items,
        },
        else => types.Result{
            .errors_count = controller.errors_count,
            .items_completed = 0,
            .total_items = 0,
        },
    };
}
