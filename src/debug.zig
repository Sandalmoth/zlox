const std = @import("std");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

const _value = @import("value.zig");

pub fn disassembleChunk(chunk: Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    std.debug.print("{:4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{:4} ", .{chunk.lines.items[offset]});
    }

    const byte: u8 = chunk.code.items[offset];
    if (byte >= std.meta.fields(OpCode).len) {
        std.debug.print("Unknown opcode {}\n", .{byte});
    }

    const instruction: OpCode = @enumFromInt(byte);
    switch (instruction) {
        .CONST => return constantInstruction("OP_CONST", chunk, offset),
        .CONST_LONG => return constantLongInstruction("OP_CONST_LONG", chunk, offset),
        .NIL => return simpleInstruction("OP_NIL", offset),
        .TRUE => return simpleInstruction("OP_TRUE", offset),
        .FALSE => return simpleInstruction("OP_FALSE", offset),
        .POP => return simpleInstruction("OP_POP", offset),
        .GET_GLOBAL => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .DEFINE_GLOBAL => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .EQUAL => return simpleInstruction("OP_EQUAL", offset),
        .GT => return simpleInstruction("OP_GT", offset),
        .LT => return simpleInstruction("OP_LT", offset),
        .ADD => return simpleInstruction("OP_ADD", offset),
        .SUB => return simpleInstruction("OP_SUB", offset),
        .MUL => return simpleInstruction("OP_MUL", offset),
        .DIV => return simpleInstruction("OP_DIV", offset),
        .NOT => return simpleInstruction("OP_NOT", offset),
        .NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .PRINT => return simpleInstruction("OP_PRINT", offset),
        .RETURN => return simpleInstruction("OP_RETURN", offset),
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {:4} '", .{ name, constant });
    _value.printValue(chunk.constants.values.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn constantLongInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    // take three bytes, reinterpret as u24
    // maybe a pointerCast would be cleaner?
    var bytes: [3]u8 = undefined;
    std.mem.copy(u8, &bytes, chunk.code.items[offset + 1 .. offset + 4]);
    const constant = std.mem.readIntNative(u24, &bytes);
    std.debug.print("{s:<16} {:4} '", .{ name, constant });
    _value.printValue(chunk.constants.values.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 4;
}
