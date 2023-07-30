const std = @import("std");

pub const ValueType = enum {
    BOOL,
    NIL,
    NUMBER,
};

pub const Value = union(ValueType) {
    BOOL: bool,
    NIL: void,
    NUMBER: f64,
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
    }
}

pub fn valuesEqual(a: Value, b: Value) bool {
    // NOTE what if we had an actual pointer in a value?
    return std.meta.eql(a, b);
}
