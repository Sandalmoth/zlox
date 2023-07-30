const std = @import("std");

const _scanner = @import("scanner.zig");
const Scanner = _scanner.Scanner;

pub fn compile(alloc: std.mem.Allocator, source: []const u8) void {
    var scanner = Scanner.init(alloc, source);

    var line: u32 = 0; // since line always starts at 1, this will default to printing the first line
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{:4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{:2} '{s}'\n", .{
            @intFromEnum(token.type),
            token.lexeme,
        });

        if (token.type == .EOF) {
            break;
        }
    }
}
