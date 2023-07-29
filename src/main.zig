const std = @import("std");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

const _debug = @import("debug.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    var chunk = Chunk.init(alloc);
    defer chunk.deinit();

    {
        const constant = chunk.addConstant(1.2);
        chunk.writeOp(.CONST, 123);
        std.debug.assert(constant < std.math.maxInt(u8));
        chunk.write(@truncate(constant), 123);
    }

    {
        const constant = chunk.addConstant(3.4);
        chunk.writeOp(.CONST_LONG, 456);
        std.debug.assert(constant < std.math.maxInt(u24));
        chunk.writeLong(u24, @as(u24, @truncate(constant)), 456);
    }

    chunk.writeOp(.RETURN, 123);

    _debug.disassembleChunk(chunk, "test chunk");
}
