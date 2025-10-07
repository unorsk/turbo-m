const std = @import("std");
const turbo_m = @import("turbo_m");

const log = std.log.scoped(.main);

const Mode = enum {
    tui,
    cli,
    help,
};

const ParsedArgs = struct {
    mode: Mode,
    file_path: ?[]const u8 = null,
};

fn parseArgs(alloc: std.mem.Allocator) !ParsedArgs {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    _ = args.skip();

    if (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "tui")) {
            const file_path = args.next();
            return .{ .mode = .tui, .file_path = file_path };
        } else if (std.mem.eql(u8, arg, "help") or std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            return .{ .mode = .help };
        }
    }

    // Default to CLI mode
    return .{ .mode = .cli };
}

fn loadItemsFromFile(alloc: std.mem.Allocator, file_path: []const u8) ![]turbo_m.Item {
    const file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    const file_contents = try file.readToEndAlloc(alloc, std.math.maxInt(usize));
    defer alloc.free(file_contents);

    var lines = std.mem.splitSequence(u8, file_contents, "\n");

    var results = std.array_list.Managed(turbo_m.Item).init(alloc);
    errdefer results.deinit();

    while (lines.next()) |line| {
        if (line.len == 0) continue; // Skip empty lines

        var parts = std.mem.splitSequence(u8, line, "##");
        const first = parts.first();
        const last = parts.next() orelse "";

        // Duplicate strings to ensure they remain valid after file_contents is freed
        const back_copy = try alloc.dupe(u8, first);
        const front_copy = try alloc.dupe(u8, last);

        try results.append(turbo_m.Item{ .front = front_copy, .back = back_copy });
    }

    return try results.toOwnedSlice();
}

fn printUsage() void {
    std.debug.print(
        \\turbo-m - Spaced Repetition System
        \\
        \\Usage:
        \\  turbo_m [mode] [options]
        \\
        \\Modes:
        \\  tui <file>  Run the interactive TUI interface with items from file
        \\  help        Show this help message
        \\  (default)   Run in CLI mode (not yet implemented)
        \\
        \\Examples:
        \\  turbo_m tui items.txt
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

    const parsed_args = try parseArgs(alloc);

    switch (parsed_args.mode) {
        .help => {
            printUsage();
            return;
        },
        .tui => {
            if (parsed_args.file_path) |file_path| {
                const items = try loadItemsFromFile(alloc, file_path);
                defer {
                    for (items) |item| {
                        alloc.free(item.front);
                        alloc.free(item.back);
                    }
                    alloc.free(items);
                }
                const result = try turbo_m.tui.run(alloc, items);

                log.info("Training session completed:", .{});
                log.info("  Items completed: {}/{}", .{ result.items_completed, result.total_items });
                log.info("  Errors: {}", .{result.errors_count});
            } else {
                log.err("Expected a path to a deck: turbo-m tui <file>", .{});
                return;
            }
        },
        .cli => {
            std.debug.print("CLI mode not yet implemented. Use 'turbo_m tui <file>' or 'turbo_m help'\n", .{});
            return error.NotImplemented;
        },
    }
}
