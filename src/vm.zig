const std = @import("std");

const _chunk = @import("chunk.zig");
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;

const _value = @import("value.zig");
const Value = _value.Value;
const Obj = _value.Obj;
const ObjString = _value.ObjString;

const _debug = @import("debug.zig");

const _compiler = @import("compiler.zig");

const _object = @import("object.zig");
const ObjFunction = _object.ObjFunction;

pub const InterpretResult = enum { ok, compile_error, runtime_error };

const stack_max = 256;
const frames_max = 16;
const debug_trace_execution: bool = true;

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,
};

pub const VM = struct {
    // going outside the book a bit:
    // i'd rather not make the vm static
    // hopefully doesn't complicate much
    alloc: std.mem.Allocator,

    chunk: *Chunk,
    ip: [*]u8,

    frames: []CallFrame,
    frame_count: usize,

    stack: []Value,
    stack_top: [*]Value,

    globals: std.StringHashMap(Value),
    // NOTE i decided to skip string interning
    // it's cool, but not that exciting to me right now
    objects: ?*Obj,

    pub fn init(alloc: std.mem.Allocator) VM {
        // it's kinda ugly leaving undefined fields
        // but, interpret will make sure it's fine for chunk and ip
        var vm = VM{
            .alloc = alloc,
            .chunk = undefined, // set by interpret (the entry point)
            .ip = undefined, // set by interpret
            .frames = alloc.alloc(CallFrame, frames_max) catch unreachable,
            .frame_count = 0,
            .stack = alloc.alloc(Value, stack_max) catch unreachable,
            .stack_top = undefined,
            .globals = std.StringHashMap(Value).init(alloc),
            .objects = null,
        };
        vm.resetStack(); // sets stack_top
        return vm;
    }

    pub fn deinit(vm: *VM) void {
        vm.alloc.free(vm.frames);
        vm.alloc.free(vm.stack);
        vm.globals.deinit(); // note, keys are freed by freeObjects
        vm.freeObjects();
        vm.* = undefined;
    }

    fn freeObjects(vm: *VM) void {
        var obj = vm.objects;
        while (obj != null) {
            const next = obj.?.next;
            vm.freeObject(obj.?);
            obj = next;
        }
    }

    fn freeObject(vm: *VM, obj: *Obj) void {
        switch (obj.type) {
            .FUNCTION => {
                var function = @as(*ObjFunction, @ptrCast(obj));
                function.chunk.deinit();
                vm.alloc.destroy(function);
            },
            .STRING => {
                const string = @as(*ObjString, @ptrCast(obj));
                vm.alloc.free(string.chars[0..string.len]);
                vm.alloc.destroy(string);
            },
        }
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
        var function = _compiler.compile(vm.alloc, &vm.objects, source);
        if (function == null) {
            return .compile_error;
        }

        vm.push(Value{ .OBJ = @ptrCast(function.?) });
        const frame = &vm.frames[vm.frame_count];
        vm.frame_count += 1;
        frame.function = function.?;
        frame.ip = frame.function.chunk.code.items.ptr;
        frame.slots = vm.stack.ptr;

        return vm.run();
    }

    /// run reads bytecode and dispatches functions
    /// then op_.* functions execute the operation
    /// tail recursion is enforced to prevent stack overflow
    fn run(vm: *VM) InterpretResult {
        // note, the mutual recursion eliminates the need for a loop
        const frame = &vm.frames[vm.frame_count - 1];

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
                frame.function.chunk,
                @intCast(@intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr)),
            );
        }

        const byte = frame.ip[0];
        frame.ip += 1;
        std.debug.assert(byte < std.meta.fields(OpCode).len);
        const instruction: OpCode = @enumFromInt(byte);

        switch (instruction) {
            .CONST => return @call(.always_tail, op_CONST, .{vm}),
            .CONST_LONG => @panic("CONST_LONG not implemented"),
            .NIL => return @call(.always_tail, op_NIL, .{vm}),
            .TRUE => return @call(.always_tail, op_TRUE, .{vm}),
            .FALSE => return @call(.always_tail, op_FALSE, .{vm}),
            .POP => return @call(.always_tail, op_POP, .{vm}),
            .GET_LOCAL => return @call(.always_tail, op_GET_LOCAL, .{vm}),
            .GET_GLOBAL => return @call(.always_tail, op_GET_GLOBAL, .{vm}),
            .DEFINE_GLOBAL => return @call(.always_tail, op_DEFINE_GLOBAL, .{vm}),
            .SET_LOCAL => return @call(.always_tail, op_SET_LOCAL, .{vm}),
            .SET_GLOBAL => return @call(.always_tail, op_SET_GLOBAL, .{vm}),
            .EQUAL => return @call(.always_tail, op_EQUAL, .{vm}),
            .GT => return @call(.always_tail, op_GT, .{vm}),
            .LT => return @call(.always_tail, op_LT, .{vm}),
            .ADD => return @call(.always_tail, op_ADD, .{vm}),
            .SUB => return @call(.always_tail, op_SUB, .{vm}),
            .MUL => return @call(.always_tail, op_MUL, .{vm}),
            .DIV => return @call(.always_tail, op_DIV, .{vm}),
            .NOT => return @call(.always_tail, op_NOT, .{vm}),
            .NEGATE => return @call(.always_tail, op_NEGATE, .{vm}),
            .PRINT => return @call(.always_tail, op_PRINT, .{vm}),
            .JUMP => return @call(.always_tail, op_JUMP, .{vm}),
            .JUMP_IF_FALSE => return @call(.always_tail, op_JUMP_IF_FALSE, .{vm}),
            .LOOP => return @call(.always_tail, op_LOOP, .{vm}),
            .RETURN => return @call(.always_tail, op_RETURN, .{vm}),
        }

        // we mutually recur between run and ops
        // until returning an InterpretResult
        // so getting here should be impossible
        unreachable;
    }

    fn op_CONST(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte = frame.ip[0];
        frame.ip += 1;
        const constant = frame.function.chunk.constants.values.items[byte];
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

    fn op_POP(vm: *VM) InterpretResult {
        _ = vm.pop();
        return @call(.always_tail, run, .{vm});
    }

    fn op_GET_LOCAL(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const slot = frame.ip[0];
        frame.ip += 1;
        vm.push(frame.slots[@intCast(slot)]);

        return @call(.always_tail, run, .{vm});
    }

    fn op_SET_LOCAL(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const slot = frame.ip[0];
        frame.ip += 1;
        frame.slots[@intCast(slot)] = vm.peek(0);

        return @call(.always_tail, run, .{vm});
    }

    fn op_GET_GLOBAL(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte = frame.ip[0];
        frame.ip += 1;
        const name = vm.chunk.constants.values.items[byte].asString();
        const value = vm.globals.get(name.chars[0..name.len]) orelse {
            vm.runtimeError("Undefined variable '{s}'", .{name.chars[0..name.len]});
            return .runtime_error;
        };
        vm.push(value);

        return @call(.always_tail, run, .{vm});
    }

    fn op_DEFINE_GLOBAL(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte = frame.ip[0];
        frame.ip += 1;
        const name = vm.chunk.constants.values.items[byte].asString();
        vm.globals.put(name.chars[0..name.len], vm.peek(0)) catch unreachable;
        _ = vm.pop();

        return @call(.always_tail, run, .{vm});
    }

    fn op_SET_GLOBAL(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte = frame.ip[0];
        frame.ip += 1;
        const name = vm.chunk.constants.values.items[byte].asString();
        // I guess if we remove this check then we get implicit variable decls
        if (vm.globals.contains(name.chars[0..name.len])) {
            vm.globals.put(name.chars[0..name.len], vm.peek(0)) catch unreachable;
        } else {
            vm.runtimeError("Undefined variable '{s}'", .{name.chars[0..name.len]});
            return .runtime_error;
        }
        // NOTE why not pop?

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

    fn concatenate(vm: *VM) void {
        const b = vm.pop().asString();
        const a = vm.pop().asString();

        const len = a.len + b.len;
        var chars = vm.alloc.alloc(u8, len) catch unreachable;
        std.mem.copy(u8, chars[0..a.len], a.chars[0..a.len]);
        std.mem.copy(u8, chars[a.len..len], b.chars[0..b.len]);
        const result = _object.takeString(vm.alloc, &vm.objects, chars);
        vm.push(Value{ .OBJ = @ptrCast(result) });
    }

    fn op_ADD(vm: *VM) InterpretResult {
        if (vm.peek(0).isString() and vm.peek(1).isString()) {
            vm.concatenate();
        } else if (vm.peek(0) == .NUMBER and vm.peek(1) == .NUMBER) {
            const b = vm.pop().NUMBER;
            const a = vm.pop().NUMBER;
            vm.push(Value{ .NUMBER = a + b });
        } else {
            vm.runtimeError("Operands must be two of either numbers or strings", .{});
            return .runtime_error;
        }

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

    fn op_PRINT(vm: *VM) InterpretResult {
        _value.printValue(vm.pop());
        std.debug.print("\n", .{});

        return @call(.always_tail, run, .{vm});
    }

    fn op_JUMP(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte1: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const byte2: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const offset = (byte1 << 8) | byte2;
        frame.ip += offset;

        return @call(.always_tail, run, .{vm});
    }

    fn op_JUMP_IF_FALSE(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte1: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const byte2: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const offset = (byte1 << 8) | byte2;
        if (isFalsy(vm.peek(0))) {
            frame.ip += offset;
        }

        return @call(.always_tail, run, .{vm});
    }

    fn op_LOOP(vm: *VM) InterpretResult {
        const frame = &vm.frames[vm.frame_count - 1];
        const byte1: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const byte2: usize = @intCast(frame.ip[0]);
        frame.ip += 1;
        const offset = (byte1 << 8) | byte2;
        frame.ip -= offset;

        return @call(.always_tail, run, .{vm});
    }

    fn op_RETURN(vm: *VM) InterpretResult {
        _ = vm;
        return .ok;
    }

    fn runtimeError(vm: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);

        const frame = &vm.frames[vm.frame_count - 1];
        const instruction: usize = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr) - 1;
        const line = frame.function.chunk.lines.items[instruction];
        std.debug.print("\n[line {}] in script\n", .{line});

        vm.resetStack();
    }
};
