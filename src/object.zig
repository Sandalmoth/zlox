const std = @import("std");

const _value = @import("value.zig");
const Obj = _value.Obj;
const ObjString = _value.ObjString;

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
        ObjString => obj.obj.type = .STRING,
        else => @compileError("Invalid type of Obj: " ++ @typeName(T)),
    }
    return obj;
}
