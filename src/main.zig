const std = @import("std");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

const _vm = @import("vm.zig");
const VM = _vm.VM;

const _debug = @import("debug.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var vm = VM.init(alloc);
    defer vm.deinit();

    var chunk = Chunk.init(alloc);
    defer chunk.deinit();

    {
        const constant = chunk.addConstant(1.2);
        chunk.writeOp(.CONST, 123);
        std.debug.assert(constant < std.math.maxInt(u8));
        chunk.write(@truncate(constant), 123);
    }

    chunk.writeOp(.RETURN, 123);

    _debug.disassembleChunk(chunk, "test chunk");
    std.debug.print("\n", .{});
    _ = vm.interpret(&chunk);
}
