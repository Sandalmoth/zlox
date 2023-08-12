const std = @import("std");

const _object = @import("object.zig");
const ObjFunction = _object.ObjFunction;

pub const ValueType = enum {
    BOOL,
    NIL,
    NUMBER,
    OBJ,
};

pub const Value = union(ValueType) {
    BOOL: bool,
    NIL: void,
    NUMBER: f64,
    OBJ: *Obj,

    pub inline fn isObjType(value: Value, t: ObjType) bool {
        return value == .OBJ and value.OBJ.type == t;
    }

    pub inline fn isString(value: Value) bool {
        return value.isObjType(.STRING);
    }

    pub inline fn isFunction(value: Value) bool {
        return value.isObjType(.FUNCTION);
    }

    pub inline fn asString(value: Value) *ObjString {
        std.debug.assert(value.isString());
        return @as(*ObjString, @ptrCast(value.OBJ));
    }

    pub inline fn asFunction(value: Value) *ObjFunction {
        std.debug.assert(value.isFunction());
        return @as(*ObjFunction, @ptrCast(value.OBJ));
    }

    pub fn asChars(value: Value) []u8 {
        std.debug.assert(value.isString());
        const string = @as(*ObjString, @ptrCast(value.OBJ));
        return string.chars[0..string.len];
    }
};

pub const ObjType = enum {
    FUNCTION,
    STRING,
};

// NOTE
// though it's not a problem here,
// if @alignOf(Obj) < @alignOf(Obj.*)
// then we'd have issues with casting
// and I wonder how that could be fixed

pub const Obj = packed struct {
    next: ?*Obj,
    type: ObjType,
    // do we need padding?
};

pub const ObjString = packed struct {
    obj: Obj,
    chars: [*]u8,
    len: usize,
};

pub const ValueArray = struct {
    values: std.ArrayList(Value),

    pub fn init(alloc: std.mem.Allocator) ValueArray {
        return ValueArray{
            .values = std.ArrayList(Value).init(alloc),
        };
    }

    pub fn deinit(va: *ValueArray) void {
        va.values.deinit();
        va.* = undefined;
    }

    pub fn write(va: *ValueArray, value: Value) void {
        va.values.append(value) catch unreachable;
    }
};

pub fn printValue(value: Value) void {
    switch (value) {
        .BOOL => |x| {
            std.debug.print("{}", .{x});
        },
        .NIL => {
            std.debug.print("nil", .{});
        },
        .NUMBER => |x| {
            std.debug.print("{}", .{x});
        },
        .OBJ => {
            printObject(value);
        },
    }
}

pub fn printObject(value: Value) void {
    std.debug.assert(value == .OBJ);

    switch (value.OBJ.type) {
        .FUNCTION => {
            printFunction(value.asFunction().*);
        },
        .STRING => {
            std.debug.print("{s}", .{value.asChars()});
        },
    }
}

pub fn printFunction(function: ObjFunction) void {
    if (function.name) |name| {
        std.debug.print("<fn {s}>", .{name.chars[0..name.len]});
    } else {
        std.debug.print("<script>", .{});
    }
}

pub fn valuesEqual(a: Value, b: Value) bool {
    // NOTE this should be guaranteed by the caller
    std.debug.assert(@as(ValueType, a) == @as(ValueType, b));
    if (a == .OBJ) {
        std.debug.assert(a.OBJ.type == b.OBJ.type);
    }

    switch (a) {
        .BOOL => return a.BOOL == b.BOOL,
        .NIL => return true,
        .NUMBER => return a.NUMBER == b.NUMBER,
        .OBJ => {
            // TODO other types
            return std.mem.eql(u8, a.asChars(), b.asChars());
            // const str_a = a.asString();
            // const str_b = b.asString();
            // return str_a.len == str_b.len and
            //     std.mem.eql(str_a.chars[0..str_a.len], str_b.chars[0..str_b.len]);
        },
    }
}
