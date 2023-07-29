const std = @import("std");

pub const Value = f64;

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
    std.debug.print("{}", .{value});
}
