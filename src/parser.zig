const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
pub const Input = @import("generics.zig").makeInput(Token);

/// Operators are used to represent the different types of expressions and stringify function for printing
pub const Operator = enum {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    BANG,
    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS,
    GREATER,
    LESS_EQUAL,
    GREATER_EQUAL,
    pub fn stringify(self: Operator) []const u8 {
        return switch (self) {
            .PLUS => "+",
            .MINUS => "-",
            .STAR => "*",
            .SLASH => "/",
            .BANG => "!",
            .BANG_EQUAL => "!=",
            .EQUAL_EQUAL => "==",
            .LESS => "<",
            .GREATER => ">",
            .LESS_EQUAL => "<=",
            .GREATER_EQUAL => ">=",
        };
    }
};

/// Statements are a wrapper around an array of expressions
pub const Statements = []*Expression;

/// Expressions are the main data structure of the parser, they are a union of all possible expressions
pub const Expression = union(enum) {
    binary: struct { left: *Expression, operator: Operator, right: *Expression },
    unary: struct { operator: Operator, right: *Expression },
    literal: union(enum) { NUMBER: f64, STRING: []const u8, NIL, TRUE, FALSE },
    variable: struct { name: []const u8, value: *Expression },
    grouping: *Expression,
    print: *Expression,
    identifier: []const u8,
    parseError: anyerror,
};

/// loops though all statements and prints them
pub fn printStatements(writer: anytype, statements: Statements) !void {
    for (statements) |current| {
        try printSingleExpression(writer, current);
        writer.writeAll("\n") catch unreachable;
    }
}

/// recursively prints an single expression tree
pub fn printSingleExpression(writer: anytype, expressionTree: *Expression) !void {
    switch (expressionTree.*) {
        .binary => |*binary| {
            try writer.print("({s} ", .{binary.operator.stringify()});
            try printSingleExpression(writer, binary.left);
            try writer.print(" ", .{});
            try printSingleExpression(writer, binary.right);
            try writer.print(")", .{});
        },
        .unary => |unary| {
            try writer.print("({s} ", .{unary.operator.stringify()});
            try printSingleExpression(writer, unary.right);
            try writer.print(")", .{});
        },
        .literal => |literal| {
            switch (literal) {
                .NUMBER => |number| {
                    //dont know of a clean way to set min precision
                    if (@ceil(number) == number) {
                        try writer.print("{d}.0", .{number});
                        return;
                    } else {
                        try writer.print("{d}", .{number});
                        return;
                    }
                },
                .STRING => |string| try writer.print("{s}", .{string}),
                .NIL => try writer.print("nil", .{}),
                .TRUE => try writer.print("true", .{}),
                .FALSE => try writer.print("false", .{}),
            }
        },
        .print => |print| {
            try writer.print("(print ", .{});
            try printSingleExpression(writer, print);
            try writer.print(")", .{});
        },
        .grouping => |grouping| {
            try writer.print("(group ", .{});
            try printSingleExpression(writer, grouping);
            try writer.print(")", .{});
        },
        .identifier => |identifier| {
            try writer.print("{s}", .{identifier});
        },
        .variable => |variable| {
            try writer.print("({s} = ", .{variable.name});
            try printSingleExpression(writer, variable.value);
            try writer.print(")", .{});
        },
        .parseError => |parseError| {
            try std.io.getStdErr().writer().print("parseError: {}", .{parseError});
            return;
        },
    }
}

/// loops though all statements and checks for errors
pub fn errorCheckStatements(statements: Statements) !void {
    for (statements) |current| {
        try errorCheckExpression(current);
    }
}

/// checks for errors in an expression
pub fn errorCheckExpression(expression_tree: *const Expression) !void {
    switch (expression_tree.*) {
        .binary => |*binary| {
            try errorCheckExpression(binary.left);
            try errorCheckExpression(binary.right);
        },
        .unary => |unary| {
            try errorCheckExpression(unary.right);
        },
        .grouping => |grouping| {
            try errorCheckExpression(grouping);
        },
        .parseError => |parseError| {
            std.debug.print("parseError: {}\n", .{parseError});
            return parseError;
        },
        .print => |print| {
            try errorCheckExpression(print);
        },
        .variable => |variable| {
            try errorCheckExpression(variable.value);
        },
        .literal => return,
        .identifier => return,
    }
}

/// main entry point for the parser, takes an lexer input and turns them into a list of expression trees AKA Statements
/// has a context array for tracking groupings and a ignore_errors flag for turning off error checking
pub fn parser(input: *Input, ignore_errors: bool) !Statements {
    var context = std.ArrayList(u8).init(std.heap.page_allocator);
    var statements = std.ArrayList(*Expression).init(std.heap.page_allocator);
    defer context.deinit();
    while (input.peek()) |c| {
        if (c.token_type == .EOF) {
            break;
        }
        const expression_tree = try expression(input, &context, false);
        try statements.append(expression_tree.?);
    }
    const expressionArray: Statements = try statements.toOwnedSlice();
    if (!ignore_errors) {
        try errorCheckStatements(expressionArray);
    }
    return expressionArray;
}

/// helper function for the parser, takes an input and a context and returns an expression tree or null if there is no more input
/// has a flag for whether we want to drop the semicolon or not for nested calls of expression later
fn expression(input: *Input, context: *std.ArrayList(u8), keepSemicolon: bool) !?*Expression {
    var expression_helper: ?*Expression = null;
    while (input.peek()) |c| {
        if (c.token_type == .SEMICOLON) {
            if (!keepSemicolon) {
                _ = input.next();
            }
            break;
        }
        expression_helper = expressionHelper(input, context, expression_helper);
        if (@import("builtin").is_test) {
            std.debug.print("Expression:", .{});
            printSingleExpression(std.io.getStdOut().writer(), expression_helper.?) catch @panic("error printing expression");
            std.debug.print("\n", .{});
        }
    }
    return expression_helper;
}

/// special expression handler for grouping expressions (i.e. parenthesis) needs to track the context
fn groupExpression(input: *Input, context: *std.ArrayList(u8)) *Expression {
    var expression_helper: ?*Expression = null;
    context.append('(') catch unreachable;
    while (input.peek()) |c| {
        if (c.token_type == .RIGHT_PAREN) {
            _ = input.next();
            break;
        }
        expression_helper = expressionHelper(input, context, expression_helper);
        if (@import("builtin").is_test) {
            std.debug.print("Tester:", .{});
            printSingleExpression(std.io.getStdOut().writer(), expression_helper.?) catch @panic("error printing expression");
            std.debug.print("\n", .{});
        }
    }

    if (context.pop() != '(' or expression_helper.?.* == .parseError) {
        if (expression_helper.?.*.parseError == error.unexpectedEOF) {
            return makeNewExpressionPointer(Expression{ .parseError = error.Unterminatedgroup }).?;
        }
    }
    return makeNewExpressionPointer(Expression{ .grouping = expression_helper.? }).?;
}

/// helper function for the parser, takes an input and a context and returns an expression tree or null if there is no more input
/// has an optional previous expression to handle precedence, uses a switch statement to handle all the different types of expressions
fn expressionHelper(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression) ?*Expression {
    if (input.peek()) |c| {
        _ = input.next();
        const new_expression = switch (c.token_type) {
            .TRUE, .FALSE, .NIL, .NUMBER, .STRING => makeLiteral(c),
            .LEFT_PAREN => groupExpression(input, context),
            .RIGHT_PAREN => makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedGroup }).?,
            .LEFT_BRACE => @panic("TODO"),
            .RIGHT_BRACE => @panic("TODO"),
            .SEMICOLON => return null,
            .COMMA => @panic("TODO"),
            .PLUS => makeBinary(input, context, previous, .PLUS, plusOrMinusPercedence),
            .STAR => makeBinary(input, context, previous, .STAR, multipleOrDividePercedence),
            .SLASH => makeBinary(input, context, previous, .SLASH, multipleOrDividePercedence),
            .BANG => makeUnary(input, context, .BANG),
            .BANG_EQUAL => makeBinary(input, context, previous, .BANG_EQUAL, comparePercedence),
            .EQUAL => @panic("TODO"),
            .EQUAL_EQUAL => makeBinary(input, context, previous, .EQUAL_EQUAL, comparePercedence),
            .LESS => makeBinary(input, context, previous, .LESS, comparePercedence),
            .GREATER => makeBinary(input, context, previous, .GREATER, comparePercedence),
            .LESS_EQUAL => makeBinary(input, context, previous, .LESS_EQUAL, comparePercedence),
            .GREATER_EQUAL => makeBinary(input, context, previous, .GREATER_EQUAL, comparePercedence),
            .IDENTIFIER => makeIdentifier(c),
            .AND => @panic("TODO"),
            .CLASS => @panic("TODO"),
            .ELSE => @panic("TODO"),
            .FOR => @panic("TODO"),
            .FUN => @panic("TODO"),
            .IF => @panic("TODO IF"),
            .OR => @panic("TODO"),
            .PRINT => makePrint(input, context),
            .RETURN => @panic("TODO"),
            .SUPER => @panic("TODO"),
            .THIS => @panic("TODO"),
            .VAR => makeVariable(input, context),
            .WHILE => @panic("TODO"),
            .DOT => @panic("TODO"),
            .INVALID_TOKEN => @panic("TODO"),
            .UNTERMINATED_STRING => @panic("TODO"),
            .EOF => handleEOF(context, previous),
            .MINUS => handleMinus(input, context, previous),
            // i dont like this
        };
        return new_expression;
    }

    return null;
}

/// expression handler for variables, needs to track the context for error handling and sub expression call
/// variables should be of the form var x = Expression or var x
/// this goes through each token in the stream and if it doesnt match the expected format it returns an error
/// once it gets to the Expression it calls the main Expression function to parse it as a "new expression" which is then assigned to the var expression node
fn makeVariable(input: *Input, context: *std.ArrayList(u8)) *Expression {
    const name = input.next() orelse return handleEOF(context, null);
    if (name.token_type != .IDENTIFIER) {
        return makeNewExpressionPointer(Expression{ .parseError = error.UnexpectedToken }).?;
    }
    const equal = input.next() orelse return handleEOF(context, null);
    if (equal.token_type != .EQUAL) {
        if (equal.token_type == .SEMICOLON) {
            return makeNewExpressionPointer(Expression{ .variable = .{ .name = name.lexeme, .value = makeNewExpressionPointer(Expression{ .literal = .{ .NIL = {} } }).? } }).?;
        }
        return makeNewExpressionPointer(Expression{ .parseError = error.UnexpectedToken }).?;
    }

    const value = (try expression(input, context, true)).?;
    if (value.* == .parseError and value.*.parseError != error.unexpectedEOF) {
        return makeNewExpressionPointer(Expression{ .parseError = error.UnexpectedToken }).?;
    }
    return makeNewExpressionPointer(Expression{ .variable = .{ .name = name.lexeme, .value = value } }).?;
}

/// expression handler for print statements, needs to track the context for sub expression calls
/// print should be of the form print Expression
/// it calls the main Expression function to parse it as a "new expression" which is then assigned to the print expression node
fn makePrint(input: *Input, context: *std.ArrayList(u8)) *Expression {
    var right = try expression(input, context, true);
    if (right == null or right.?.* == .parseError) {
        right = makeNewExpressionPointer(Expression{ .parseError = error.UnexpectedEOF }).?;
    }
    return makeNewExpressionPointer(Expression{ .print = right.? }).?;
}

/// expression handler for EOF, needs to track the context for error handling and sub expression call
/// checks if the context is empty and if it is returns the previous expression if there is one
/// if there is no previous expression it returns an error
/// if the conext is not empty returns an error
fn handleEOF(context: *std.ArrayList(u8), previous: ?*Expression) *Expression {
    if (context.items.len == 0) {
        return previous orelse return makeNewExpressionPointer(Expression{ .parseError = error.unexpectedEOF }).?;
    }
    if (previous) |prev| {
        errorCheckExpression(prev) catch return prev;
    }
    return makeNewExpressionPointer(Expression{ .parseError = error.unexpectedEOF }).?;
}

/// expression handler for unary minus, needs to track the context for error handling and sub expression call
/// checks if the previous expression exists and if it does we assume the intent is a binary expression
/// if previous doesnt exist we assume the intent is the unary form
fn handleMinus(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression) *Expression {
    if (previous) |_| {
        return makeBinary(input, context, previous, .MINUS, plusOrMinusPercedence);
    } else {
        return makeUnary(input, context, .MINUS);
    }
}

/// expression handler for identifiers, just takes the lexme from the token and turns it into an expression
fn makeIdentifier(token: Token) *Expression {
    const identifier = token.lexeme;
    return makeNewExpressionPointer(Expression{ .identifier = identifier }).?;
}

/// expression handler for literals
/// takes the token and turns it into an expression
fn makeLiteral(token: Token) ?*Expression {
    const tokenType = token.token_type;
    const new_expression = switch (tokenType) {
        .NUMBER => Expression{ .literal = .{ .NUMBER = token.literal.?.number } },
        .STRING => Expression{ .literal = .{ .STRING = token.literal.?.string } },
        .NIL => Expression{ .literal = .{ .NIL = {} } },
        .TRUE => Expression{ .literal = .{ .TRUE = {} } },
        .FALSE => Expression{ .literal = .{ .FALSE = {} } },
        else => null,
    };
    return makeNewExpressionPointer(new_expression);
}

/// experssion handler for when we need to convert true and false to there expression counter parts, or turn numbers into there truthy value
fn convertToBoolean(new_expression: *Expression) *Expression {
    switch (new_expression.*) {
        .literal => |literal| {
            switch (literal) {
                .NUMBER => |number| {
                    if (number == 0) {
                        return makeNewExpressionPointer(Expression{ .literal = .{ .FALSE = {} } }).?;
                    } else {
                        return makeNewExpressionPointer(Expression{ .literal = .{ .TRUE = {} } }).?;
                    }
                },
                .TRUE => return makeNewExpressionPointer(Expression{ .literal = .{ .TRUE = {} } }).?,
                .FALSE => return makeNewExpressionPointer(Expression{ .literal = .{ .FALSE = {} } }).?,
                else => {},
            }
        },
        else => {},
    }
    return new_expression;
}

/// expession handler for unary functions, first makes the right hand side of the expression, then converts it to a boolean if the operator is a bang
/// then makes a new expression with the operator and the right hand side
fn makeUnary(input: *Input, context: *std.ArrayList(u8), operator: Operator) *Expression {
    var right = expressionHelper(input, context, null) orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedUnary }).?;
    if (operator == .BANG) {
        right = convertToBoolean(right);
    }
    return makeNewExpressionPointer(Expression{ .unary = .{ .operator = operator, .right = right } }).?;
}

/// expression handler for binary functions, first makes the right hand side of the expression, then using our previous expression and operator
/// make make a new expression, afterwards we want to correct precedence so we call rotateIfPrecedenceMismatch with our compare func
fn makeBinary(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression, operator: Operator, comptime comparefunc: anytype) *Expression {
    var right = expressionHelper(input, context, null) orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    if (right.* == .parseError) {
        right = makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    }

    var new_expression = makeNewExpressionPointer(Expression{ .binary = .{
        .left = previous orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?,
        .operator = operator,
        .right = right,
    } }).?;
    new_expression = rotateIfPrecedenceMismatch(previous, new_expression, comparefunc);
    return new_expression;
}
/// tracks precedence mismatchs
const precedence = enum {
    tooLow,
    correct,
    tooHigh,
};

/// plus or minus precedence is only ever correct with itself or when it is higher then star or slash in the tree
/// otherwise it is too high
fn plusOrMinusPercedence(prev: ?*Expression) precedence {
    if (prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS or prev.?.binary.operator == .STAR or prev.?.binary.operator == .SLASH) {
        return .correct;
    }
    return .tooHigh;
}

/// multiple or divide precedence is only ever correct with itself
/// it is too low if it is higher then plus or minus in the tree
/// otherwise it is too high
fn multipleOrDividePercedence(prev: ?*Expression) precedence {
    if (prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS) {
        return .tooLow;
    } else if (prev.?.binary.operator == .STAR or prev.?.binary.operator == .SLASH) {
        return .correct;
    }

    return .tooHigh;
}

/// compare precedence is always correct
fn comparePercedence(_: ?*Expression) precedence {
    return .correct;
}

/// helper function for binary expressions, takes a previous expression and a new expression and rotates them if they have different precedence
/// uses a switch statement to handle all the different types of expressions
/// if the new expression is a binary expression it calls the precedence function to check if it is correct
/// if it is correct it calls itself again with the new expression as the previous expression
/// if it is too high or too low it calls itself again with the new expression as the previous expression
/// rotates by taking right side of previous assigning it the the left side of the new expression and assigning the right side of the new expression to the previous expression
fn rotateIfPrecedenceMismatch(previous: ?*Expression, to_sort_expression: *Expression, comptime precedenceFn: anytype) *Expression {
    var new_expression = to_sort_expression;
    if (previous) |prev| {
        if (prev.* == .binary) {
            switch (@call(.auto, precedenceFn, .{prev})) {
                .correct => {},
                .tooHigh, .tooLow => {
                    const temp = prev.binary.right;
                    prev.binary.right = new_expression;
                    new_expression.*.binary.left = temp;
                    new_expression = prev;
                },
            }
        }
    } else {
        return makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    }

    return new_expression;
}

/// helper function for unary expressions, takes a token type and returns the corresponding operator
fn tokenTypeIsOperator(token_type: TokenType) ?Operator {
    return switch (token_type) {
        .PLUS => Operator.PLUS,
        .MINUS => Operator.MINUS,
        .STAR => Operator.STAR,
        .SLASH => Operator.SLASH,
        .BANG => Operator.BANG,
        .BANG_EQUAL => Operator.BANG_EQUAL,
        .EQUAL_EQUAL => Operator.EQUAL_EQUAL,
        .LESS => Operator.LESS,
        .GREATER => Operator.GREATER,
        .LESS_EQUAL => Operator.LESS_EQUAL,
        .GREATER_EQUAL => Operator.GREATER_EQUAL,
        else => null,
    };
}

/// helper function for making a new expression pointer
fn makeNewExpressionPointer(new_expression: ?Expression) ?*Expression {
    if (new_expression) |_| {
        const new_expression_pointer = std.heap.page_allocator.create(Expression) catch unreachable;
        new_expression_pointer.* = new_expression.?;
        return new_expression_pointer;
    } else {
        return null;
    }
}

test "parserHappy" {
    // array of array of strings
    const TestCases = struct {
        input: []const u8,
        expected_output: []const u8,
    };
    const test_input = [_]TestCases{
        TestCases{ .input = "1 * 2", .expected_output = "(* 1.0 2.0)\n" },
        TestCases{ .input = "52 + 80 - 94", .expected_output = "(- (+ 52.0 80.0) 94.0)\n" },
        TestCases{ .input = "78 - 93 * 79 - 32", .expected_output = "(- (- 78.0 (* 93.0 79.0)) 32.0)\n" },
        TestCases{ .input = " \"hello\" + \"world\"", .expected_output = "(+ hello world)\n" },
        TestCases{ .input = "(-30 + 65) * (46 * 46) / (92 + 29)", .expected_output = "(/ (* (group (+ (- 30.0) 65.0)) (group (* 46.0 46.0))) (group (+ 92.0 29.0)))\n" },
        TestCases{ .input = "83 < 99 < 115", .expected_output = "(< (< 83.0 99.0) 115.0)\n" },
        TestCases{ .input = "87 <= 179", .expected_output = "(<= 87.0 179.0)\n" },
        TestCases{ .input = "print \"Hello, World!\";\n print 42;", .expected_output = "(print Hello, World!)\n(print 42.0)\n" },
    };
    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = Input{ .source = tokens };
        const expression_tree = try parser(&input, false);
        var buffer: [1024]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        try printStatements(writer, expression_tree);
        try std.testing.expectEqualStrings(test_case.expected_output, stream.buffer[0..stream.pos]);
        std.debug.print("\n\n\n", .{});
    }
}

test "parserUnhappy" {
    const TestCases = struct {
        input: []const u8,
        expected_error: anyerror,
    };
    const test_input = [_]TestCases{
        TestCases{ .input = "(72 +)", .expected_error = error.UnterminatedBinary },
        TestCases{ .input = "(foo ", .expected_error = error.Unterminatedgroup },
        TestCases{ .input = "+", .expected_error = error.UnterminatedBinary },
        TestCases{ .input = "// Print statements expect an expression\n // So, this program leads to a compilation error\n print;", .expected_error = error.UnexpectedEOF },
    };

    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = Input{ .source = tokens };
        const expression_tree = parser(&input, false);
        try std.testing.expectError(test_case.expected_error, expression_tree);
        std.debug.print("\n\n\n", .{});
    }
}
