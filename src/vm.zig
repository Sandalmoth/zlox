const std = @import("std");

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;
const OpCode = _chunk.OpCode;

const _value = @import("value.zig");
const Value = _value.Value;

const _debug = @import("debug.zig");

pub const InterpretResult = enum { ok, compile_error, runtime_error };

const stack_max = 256;
const debug_trace_execution: bool = true;

pub const VM = struct {
    // going outside the book a bit:
    // i'd rather not make the vm static
    // hopefully doesn't complicate much
    alloc: std.mem.Allocator,

    chunk: *Chunk,
    ip: [*]u8,
    stack: []Value,
    stack_top: [*]Value,

    pub fn init(alloc: std.mem.Allocator) VM {
        // it's kinda ugly leaving undefined fields
        // but, interpret will make sure it's fine for chunk and ip
        var vm = VM{
            .alloc = alloc,
            .chunk = undefined,
            .ip = undefined,
            .stack = alloc.alloc(Value, stack_max) catch unreachable,
            .stack_top = undefined,
        };
        vm.resetStack();
        return vm;
    }

    pub fn deinit(vm: *VM) void {
        vm.alloc.free(vm.stack);
        vm.* = undefined;
    }

    pub fn resetStack(vm: *VM) void {
        vm.stack_top = vm.stack.ptr;
    }

    pub fn push(vm: *VM, value: Value) void {
        vm.stack_top[0] = value;
        vm.stack_top += 1;
    }

    pub fn pop(vm: *VM) Value {
        vm.stack_top -= 1;
        return vm.stack_top[0];
    }

    pub fn interpret(vm: *VM, chunk: *Chunk) InterpretResult {
        vm.chunk = chunk;
        vm.ip = vm.chunk.code.items.ptr;
        return vm.run();
    }

    /// run reads bytecode and dispatches functions
    /// then op_.* functions execute the operation
    /// tail recursion is enforced to prevent stack overflow
    fn run(vm: *VM) InterpretResult {
        while (true) {
            if (debug_trace_execution) {
                std.debug.print("          ", .{});
                var slot = vm.stack.ptr;
                while (slot != vm.stack_top) : (slot += 1) {
                    std.debug.print("[ ", .{});
                    _value.printValue(slot[0]);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = _debug.disassembleInstruction(
                    vm.chunk.*,
                    @intCast(@intFromPtr(vm.ip) - @intFromPtr(vm.chunk.code.items.ptr)),
                );
            }

            const byte = vm.ip[0];
            vm.ip += 1;
            std.debug.assert(byte < std.meta.fields(OpCode).len);
            const instruction: OpCode = @enumFromInt(byte);

            switch (instruction) {
                .CONST => return @call(.always_tail, op_CONST, .{vm}),
                .CONST_LONG => @panic("CONST_LONG not implemented"),
                .NEGATE => return @call(.always_tail, op_NEGATE, .{vm}),
                .RETURN => return @call(.always_tail, op_RETURN, .{vm}),
            }
        }
    }

    fn op_CONST(vm: *VM) InterpretResult {
        const byte = vm.ip[0];
        vm.ip += 1;
        const constant = vm.chunk.constants.values.items[byte];
        vm.push(constant);
        // _value.printValue(constant);
        // std.debug.print("\n", .{});

        return @call(.always_tail, run, .{vm});
    }

    fn op_NEGATE(vm: *VM) InterpretResult {
        vm.push(-vm.pop());

        return @call(.always_tail, run, .{vm});
    }

    fn op_RETURN(vm: *VM) InterpretResult {
        _value.printValue(vm.pop());
        std.debug.print("\n", .{});
        return .ok;
    }
};
