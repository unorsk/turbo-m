const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const types = @import("types.zig");

const log = std.log.scoped(.tui);

const Model = struct {
    items: []const types.Item,
    current_index: usize = 0,
    errors_count: u8 = 0,
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
                } else if (key.matches(vaxis.Key.enter, .{})) {
                    const is_correct = true;

                    if (is_correct) {
                        if (self.current_index < self.items.len - 1) {
                            self.current_index += 1;
                        } else {
                            ctx.quit = true;
                            return;
                        }
                    } else {
                        self.errors_count += 1;
                    }

                    self.text_field.buf.clearRetainingCapacity();
                    return ctx.consumeAndRedraw();
                }
            },
            .focus_in => return ctx.requestFocus(self.text_field.widget()),
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        const max_size = ctx.max.size();

        if (self.current_index >= self.items.len) {
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

        const current_item = self.items[self.current_index];

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

    const model = try alloc.create(Model);
    defer alloc.destroy(model);

    model.* = .{
        .items = items,
        .current_index = 0,
        .errors_count = 0,
        .text_field = vxfw.TextField.init(alloc, &app.vx.unicode),
        .ok = .{
            .label = "Ok",
            .onClick = Model.onClick,
            .userdata = model,
        },
        .cancel = .{
            .label = "Don't remember",
            .onClick = Model.onClick,
            .userdata = model,
        },
    };
    defer model.text_field.deinit();

    try app.run(model.widget(), .{});

    return types.Result{
        .errors_count = model.errors_count,
        .items_completed = model.current_index + 1,
        .total_items = items.len,
    };
}
