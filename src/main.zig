const std = @import("std");

const log = std.log.scoped(.main);

const root = @import("root.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) {
            log.err("memory leak", .{});
        }
    }
    const alloc = gpa.allocator();

    const items = [_]root.Item{
        root.Item{ .front = "Hello", .back = "Hola" },
        root.Item{ .front = "Bye", .back = "Adios" },
        root.Item{ .front = "Thank you", .back = "Gracias" },
    };

    _ = try root.train_loop(alloc, &items);
}
