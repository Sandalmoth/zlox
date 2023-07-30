const std = @import("std");

const _chunk = @import("chunk.zig");
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;

const _value = @import("value.zig");
const Value = _value.Value;

const _debug = @import("debug.zig");

const _compiler = @import("compiler.zig");

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

    pub fn peek(vm: *VM, distance: usize) Value {
        return (vm.stack_top - 1 - distance)[0];
    }

    pub fn interpret(vm: *VM, source: []const u8) InterpretResult {
        var chunk = Chunk.init(vm.alloc);
        defer chunk.deinit();

        if (!_compiler.compile(vm.alloc, source, &chunk)) {
            return .compile_error;
        }

        vm.chunk = &chunk;
        vm.ip = vm.chunk.code.items.ptr;

        return vm.run();
    }

    /// run reads bytecode and dispatches functions
    /// then op_.* functions execute the operation
    /// tail recursion is enforced to prevent stack overflow
    fn run(vm: *VM) InterpretResult {
        // note, the mutual recursion eliminates the need for a loop
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
            .NIL => @call(.always_tail, op_NIL, .{vm}),
            .TRUE => @call(.always_tail, op_TRUE, .{vm}),
            .FALSE => @call(.always_tail, op_FALSE, .{vm}),
            .EQUAL => @call(.always_tail, op_EQUAL, .{vm}),
            .GT => @call(.always_tail, op_GT, .{vm}),
            .LT => @call(.always_tail, op_LT, .{vm}),
            .ADD => @call(.always_tail, op_ADD, .{vm}),
            .SUB => @call(.always_tail, op_SUB, .{vm}),
            .MUL => @call(.always_tail, op_MUL, .{vm}),
            .DIV => @call(.always_tail, op_DIV, .{vm}),
            .NOT => @call(.always_tail, op_NOT, .{vm}),
            .NEGATE => return @call(.always_tail, op_NEGATE, .{vm}),
            .RETURN => return @call(.always_tail, op_RETURN, .{vm}),
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

    fn op_NIL(vm: *VM) InterpretResult {
        vm.push(Value{ .NIL = {} });
        return @call(.always_tail, run, .{vm});
    }

    fn op_TRUE(vm: *VM) InterpretResult {
        vm.push(Value{ .BOOL = true });
        return @call(.always_tail, run, .{vm});
    }

    fn op_FALSE(vm: *VM) InterpretResult {
        vm.push(Value{ .BOOL = false });
        return @call(.always_tail, run, .{vm});
    }

    fn op_EQUAL(vm: *VM) InterpretResult {
        const a = vm.pop();
        const b = vm.pop();
        vm.push(Value{ .BOOL = _value.valuesEqual(a, b) });

        return @call(.always_tail, run, .{vm});
    }

    fn op_GT(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .BOOL = a > b });

        return @call(.always_tail, run, .{vm});
    }

    fn op_LT(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .BOOL = a < b });

        return @call(.always_tail, run, .{vm});
    }

    fn isFalsy(value: Value) bool {
        return value == .NIL or (value == .BOOL and !value.BOOL);
    }

    fn op_NOT(vm: *VM) InterpretResult {
        vm.push(Value{ .BOOL = isFalsy(vm.pop()) });
        return @call(.always_tail, run, .{vm});
    }

    fn op_NEGATE(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER) {
            vm.runtimeError("Operand must be a number", .{});
            return .runtime_error;
        }
        vm.push(Value{ .NUMBER = -vm.pop().NUMBER });

        return @call(.always_tail, run, .{vm});
    }

    fn op_ADD(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .NUMBER = a + b });

        return @call(.always_tail, run, .{vm});
    }

    fn op_SUB(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .NUMBER = a - b });

        return @call(.always_tail, run, .{vm});
    }

    fn op_MUL(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .NUMBER = a * b });

        return @call(.always_tail, run, .{vm});
    }

    fn op_DIV(vm: *VM) InterpretResult {
        if (vm.peek(0) != .NUMBER or vm.peek(1) != .NUMBER) {
            vm.runtimeError("Operands must be numbers", .{});
            return .runtime_error;
        }

        const b = vm.pop().NUMBER;
        const a = vm.pop().NUMBER;
        vm.push(Value{ .NUMBER = a / b });

        return @call(.always_tail, run, .{vm});
    }

    fn op_RETURN(vm: *VM) InterpretResult {
        _value.printValue(vm.pop());
        std.debug.print("\n", .{});
        return .ok;
    }

    fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);

        const instruction: usize = @intFromPtr(vm.ip) - @intFromPtr(vm.chunk.code.items.ptr) - 1;
        const line = vm.chunk.lines.items[instruction];
        std.debug.print("\n[line {}] in script\n", .{line});

        vm.resetStack();
    }
};
