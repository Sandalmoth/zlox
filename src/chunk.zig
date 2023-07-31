const std = @import("std");

const _value = @import("value.zig");
const Value = _value.Value;
const ValueArray = _value.ValueArray;

// gonna keep all-caps for operators, just to easily spot them
pub const OpCode = enum {
    CONST,
    CONST_LONG,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GT,
    LT,
    ADD,
    SUB,
    MUL,
    DIV,
    NOT,
    NEGATE,
    PRINT,
    RETURN,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(usize),
    constants: ValueArray,

    pub fn init(alloc: std.mem.Allocator) Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(alloc),
            .lines = std.ArrayList(usize).init(alloc),
            .constants = ValueArray.init(alloc),
        };
    }

    pub fn deinit(chunk: *Chunk) void {
        chunk.code.deinit();
        chunk.lines.deinit();
        chunk.constants.deinit();
        chunk.* = undefined;
    }

    pub fn write(chunk: *Chunk, byte: u8, line: usize) void {
        chunk.code.append(byte) catch unreachable;
        chunk.lines.append(line) catch unreachable;
    }

    pub fn writeOp(chunk: *Chunk, op: OpCode, line: usize) void {
        chunk.code.append(@intFromEnum(op)) catch unreachable;
        chunk.lines.append(line) catch unreachable;
    }

    /// writes any (implemented) bigger-than-byte type as a sequence of bytes
    pub fn writeLong(chunk: *Chunk, comptime T: type, long: T, line: usize) void {
        if (T == u24) {
            // write u24 to a byte array
            // maybe a pointerCast would be cleaner?
            var bytes: [3]u8 = undefined;
            std.mem.writeIntNative(u24, &bytes, long);
            for (bytes) |byte| {
                chunk.code.append(byte) catch unreachable;
                chunk.lines.append(line) catch unreachable;
            }
        } else {
            @compileError("writeLong invoked with unimplemented type: " ++ @typeName(T));
        }
    }

    pub fn addConstant(chunk: *Chunk, value: Value) usize {
        chunk.constants.write(value);
        return chunk.constants.values.items.len - 1;
    }
};
