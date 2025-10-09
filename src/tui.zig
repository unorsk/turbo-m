const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const types = @import("types.zig");
const root = @import("root.zig");

const log = std.log.scoped(.tui);

const Model = struct {
    alloc: std.mem.Allocator,
    session: root.srs.TrainingSession,
    errors_count: u8 = 0,
    text_field: vxfw.TextField,
    ok: vxfw.Button,
    cancel: vxfw.Button,
    showing_error: bool = false,
    last_match_type: types.MatchType = .exact,
    last_item: ?types.Item = null, // Store the item we just answered

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

                // If showing error, wait for any key to continue
                if (self.showing_error) {
                    self.showing_error = false;

                    // Check if session is completed
                    if (self.session.isCompleted()) {
                        ctx.quit = true;
                        return;
                    }

                    self.text_field.buf.clearRetainingCapacity();
                    return ctx.consumeAndRedraw();
                }

                if (key.matches(vaxis.Key.enter, .{})) {
                    const user_input = try self.text_field.buf.dupe();
                    defer self.alloc.free(user_input);

                    // Save the current item BEFORE submitting answer (since submitAnswer advances)
                    const current_item = self.session.currentItem();
                    self.last_item = current_item;

                    // Submit answer to training session
                    const match_type = try self.session.submitAnswer(user_input);
                    self.last_match_type = match_type;

                    if (match_type == .exact) {
                        // Exact match - clear input and continue
                        self.text_field.buf.clearRetainingCapacity();

                        // Check if completed
                        if (self.session.isCompleted()) {
                            ctx.quit = true;
                            return;
                        }
                    } else {
                        // Fuzzy or incorrect - show message with correct answer
                        self.errors_count += 1;
                        self.showing_error = true;
                    }

                    return ctx.consumeAndRedraw();
                }
            },
            // .focus_in => {
            //     // if (!self.showing_error) {
            //     return ctx.requestFocus(self.text_field.widget());
            //     // }
            // },
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        const max_size = ctx.max.size();

        // Show error view
        if (self.showing_error) {
            // Use the last item we answered (saved before submitAnswer advanced)
            const current_item = self.last_item orelse {
                // Fallback: if no last_item, show completion message
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
            };

            const status_text_str = if (self.last_match_type == .fuzzy) "≈" else "⚠️";
            const correct_text_str = try std.fmt.allocPrint(ctx.arena, "{s} Correct answer: {s}", .{ status_text_str, current_item.back });
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
            // children[0] = status_child;
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

        const current_item = self.session.currentItem() orelse {
            // If no current item, show completion message
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
        };

        const front_text: vxfw.Text = .{ .text = current_item.front };
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
                // Here we explicitly set a new maximum size constraint for the Button. A Button will
                // expand to fill its area and must have some hard limit in the maximum constraint
                .{ .width = 8, .height = 1 },
            )),
        };

        const cancel: vxfw.SubSurface = .{
            .origin = .{ .row = @divTrunc(max_size.height, 2) + 5, .col = @divTrunc(max_size.width, 2) + 31 - 8 },
            .surface = try self.cancel.draw(ctx.withConstraints(
                ctx.min,
                // Here we explicitly set a new maximum size constraint for the Button. A Button will
                // expand to fill its area and must have some hard limit in the maximum constraint
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
};

pub fn run(alloc: std.mem.Allocator, items: []const types.Item) !types.Result {
    var app = try vxfw.App.init(alloc);
    defer app.deinit();

    // Initialize training session
    var session = try root.srs.TrainingSession.init(alloc, items);
    defer session.deinit();

    const model = try alloc.create(Model);
    defer alloc.destroy(model);

    model.* = .{
        .alloc = alloc,
        .session = session,
        .errors_count = 0,
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

    return types.Result{
        .errors_count = model.errors_count,
        .items_completed = session.completedItems(),
        .total_items = session.totalItems(),
    };
}
