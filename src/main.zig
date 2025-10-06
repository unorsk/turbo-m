const std = @import("std");
const turbo_m = @import("turbo_m");

const log = std.log.scoped(.main);

const Mode = enum {
    tui,
    cli,
    help,
};

fn parseArgs(alloc: std.mem.Allocator) !Mode {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    _ = args.skip();

    if (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "tui")) {
            return .tui;
        } else if (std.mem.eql(u8, arg, "help") or std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return .help;
        }
    }

    // Default to CLI mode
    return .cli;
}

fn printUsage() void {
    std.debug.print(
        \\turbo-m - Spaced Repetition System
        \\
        \\Usage:
        \\  turbo_m [mode]
        \\
        \\Modes:
        \\  tui       Run the interactive TUI interface
        \\  help      Show this help message
        \\  (default) Run in CLI mode (not yet implemented)
        \\
        \\Examples:
        \\  turbo_m tui
        \\  turbo_m help
        \\
    , .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            log.err("memory leak detected", .{});
        }
    }
    const alloc = gpa.allocator();

    const mode = try parseArgs(alloc);

    switch (mode) {
        .help => {
            printUsage();
            return;
        },
        .tui => {
            // Sample items for testing
            const items = [_]turbo_m.Item{
                .{ .front = "Hello", .back = "Hola" },
                .{ .front = "Bye", .back = "Adios" },
                .{ .front = "Thank you", .back = "Gracias" },
                .{ .front = "Good morning", .back = "Buenos días" },
                .{ .front = "Good night", .back = "Buenas noches" },
            };

            const result = try turbo_m.tui.run(alloc, &items);

            log.info("Training session completed:", .{});
            log.info("  Items completed: {}/{}", .{ result.items_completed, result.total_items });
            log.info("  Errors: {}", .{result.errors_count});
        },
        .cli => {
            std.debug.print("CLI mode not yet implemented. Use 'turbo_m tui' or 'turbo_m help'\n", .{});
            return error.NotImplemented;
        },
    }
}
