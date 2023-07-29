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

    var constant: usize = undefined;

    constant = chunk.addConstant(1.2);
    chunk.writeOp(.CONST, 123);
    chunk.write(@truncate(constant), 123);

    constant = chunk.addConstant(3.4);
    chunk.writeOp(.CONST, 123);
    chunk.write(@truncate(constant), 123);

    chunk.writeOp(.ADD, 123);

    constant = chunk.addConstant(5.6);
    chunk.writeOp(.CONST, 123);
    chunk.write(@truncate(constant), 123);

    chunk.writeOp(.DIV, 123);
    chunk.writeOp(.NEGATE, 123);

    chunk.writeOp(.RETURN, 123);

    _debug.disassembleChunk(chunk, "test chunk");
    _ = vm.interpret(&chunk);
}
