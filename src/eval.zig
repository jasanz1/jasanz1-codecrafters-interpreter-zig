const std = @import("std");
const Expression = @import("parser.zig").Expression;
const Operator = @import("parser.zig").Operator;
const Value = union(enum) {
    STRING: []const u8,
    NUMBER: f64,
    NIL,
    TRUE,
    FALSE,
};

pub fn printValue(writer: anytype, value: *const Value) !void {
    switch (value.*) {
        .STRING => |string| try writer.print("{s}", .{string}),
        .NUMBER => |number| try writer.print("{d}", .{number}),
        .NIL => try writer.print("nil", .{}),
        .TRUE => try writer.print("true", .{}),
        .FALSE => try writer.print("false", .{}),
    }
}

pub fn evalulate(ast: *const Expression, ignore_errors: bool) Value {
    const value = eval(ast);
    if (!ignore_errors) {
        std.debug.print("parseError: huh\n", .{});
    }
    return value;
}

fn eval(ast: *const Expression) Value {
    return switch (ast.*) {
        .binary => evalBinary(ast),
        .unary => evalUnary(ast),
        .literal => |literal| switch (literal) {
            .NUMBER => Value{ .NUMBER = literal.NUMBER },
            .STRING => Value{ .STRING = literal.STRING },
            .NIL => Value{ .NIL = {} },
            .TRUE => Value{ .TRUE = {} },
            .FALSE => Value{ .FALSE = {} },
        },
        .grouping => |grouping| eval(grouping),
        .identifier => @panic("todo"),
        .parseError => @panic("todo"),
    };
}
fn evalBinary(binary: *const Expression) Value {
    const left = eval(binary.binary.left);
    const right = eval(binary.binary.right);
    switch (binary.binary.operator) {
        .PLUS => return Value{ .NUMBER = left.NUMBER + right.NUMBER },
        .MINUS => return Value{ .NUMBER = left.NUMBER - right.NUMBER },
        .STAR => return Value{ .NUMBER = left.NUMBER * right.NUMBER },
        .SLASH => return Value{ .NUMBER = left.NUMBER / right.NUMBER },
        .BANG_EQUAL => @panic("todo"),
        .EQUAL_EQUAL => @panic("todo"),
        .LESS => @panic("todo"),
        .GREATER => @panic("todo"),
        .LESS_EQUAL => @panic("todo"),
        .GREATER_EQUAL => @panic("todo"),
        else => @panic("we should never get here"),
    }
}

fn evalUnary(unary: *const Expression) Value {
    const right = eval(unary.unary.right);
    switch (unary.unary.operator) {
        .MINUS => return Value{ .NUMBER = -right.NUMBER },
        .BANG => return if (right == .TRUE) Value{ .FALSE = {} } else Value{ .TRUE = {} },
        else => @panic("we should never get here"),
    }
}
