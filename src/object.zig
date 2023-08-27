const std = @import("std");

const _value = @import("value.zig");
const Obj = _value.Obj;
const ObjString = _value.ObjString;

const _chunk = @import("chunk.zig");
const Chunk = _chunk.Chunk;

// I don't love passing the vm_objects pointer
// maybe we could bundle alloc and vm_object in a type
// but I guess that's what I get for avoiding the globals...

pub fn copyString(alloc: std.mem.Allocator, vm_objects: *?*Obj, chars: []const u8) *ObjString {
    var heap_chars: []u8 = alloc.alloc(u8, chars.len) catch unreachable;
    std.mem.copy(u8, heap_chars, chars);

    return allocateString(alloc, vm_objects, heap_chars);
}

pub fn takeString(alloc: std.mem.Allocator, vm_objects: *?*Obj, chars: []u8) *ObjString {
    // I'd probably drop this and call allocateString directly
    return allocateString(alloc, vm_objects, chars);
}

/// takes ownership of the provided slice
fn allocateString(alloc: std.mem.Allocator, vm_objects: *?*Obj, chars: []u8) *ObjString {
    var string = allocateObj(alloc, vm_objects, ObjString);
    string.chars = chars.ptr;
    string.len = chars.len;
    return string;
}

fn allocateObj(alloc: std.mem.Allocator, vm_objects: *?*Obj, comptime T: type) *T {
    var obj = alloc.create(T) catch unreachable;
    obj.obj.next = vm_objects.*;
    vm_objects.* = @ptrCast(obj);
    switch (T) {
        ObjNative => obj.obj.type = .NATIVE,
        ObjFunction => obj.obj.type = .FUNCTION,
        ObjString => obj.obj.type = .STRING,
        else => @compileError("Invalid type of Obj (JL: remember to update this function): " ++ @typeName(T)),
    }
    return obj;
}

// looks like I accidentally put the Obj family in value
// when it was supposed to go here...
// oh well I don't feel like fixing it

pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    chunk: Chunk,
    name: ?*ObjString,
};

pub fn newFunction(
    alloc: std.mem.Allocator,
    vm_objects: *?*Obj,
) *ObjFunction {
    var function = allocateObj(alloc, vm_objects, ObjFunction);
    function.arity = 0;
    function.name = null;
    function.chunk = Chunk.init(alloc);
    return function;
}

pub const NativeFn = *const (fn (u8, [*]_value.Value) _value.Value);

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

pub fn newNative(alloc: std.mem.Allocator, vm_objects: *?*Obj, function: NativeFn) *ObjNative {
    var native = allocateObj(alloc, vm_objects, ObjNative);
    native.function = function;
    return native;
}
