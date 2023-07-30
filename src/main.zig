const std = @import("std");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

const _vm = @import("vm.zig");
const VM = _vm.VM;

const _debug = @import("debug.zig");

pub fn repl(vm: *VM) !void {
    var line_buffer: [1024]u8 = undefined;
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    while (true) {
        try stdout.writer().print("> ", .{});
        const line = try stdin.reader().readUntilDelimiter(&line_buffer, '\n');
        if (line.len == 0) {
            try stdout.writer().print("\n", .{});
            return;
        }

        _ = vm.interpret(line);
    }
}

pub fn runFile(alloc: std.mem.Allocator, vm: *VM, path: []const u8) !void {
    const source = try readFile(alloc, path);
    defer alloc.free(source);

    const result = vm.interpret(source);

    switch (result) {
        .compile_error => return error.CompileError,
        .runtime_error => return error.RuntimeError,
        else => {},
    }
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{}); // default is read-only
    defer file.close();

    var buffer = try allocator.alloc(u8, try file.getEndPos());
    _ = try file.read(buffer);
    return buffer;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var vm = VM.init(alloc);
    defer vm.deinit();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try runFile(alloc, &vm, args[1]);
    } else {
        std.debug.print("Usage\n", .{});
        std.debug.print("  zlox         : launches REPL\n", .{});
        std.debug.print("  zlox [path]  : runs a file\n", .{});
    }
}
