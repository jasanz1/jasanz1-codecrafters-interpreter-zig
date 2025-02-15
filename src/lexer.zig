const std = @import("std");
pub const Input = struct {
    source: []const u8,
    index: usize,
    fn next(self: *Input) ?u8 {
        if (self.index >= self.source.len) {
            return null;
        }
        defer self.index += 1;
        return self.source[self.index];
    }
    fn peek(self: *Input) ?u8 {
        if (self.index >= self.source.len) {
            return null;
        }
        return self.source[self.index];
    }
};
pub fn makeInput(source: []const u8) Input {
    return Input{
        .source = source,
        .index = 0,
    };
}

const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
};

pub const Literal = union(enum) {
    number: usize,
    string: []const u8,
};
pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    SEMICOLON,
    COMMA,
    PLUS,
    MINUS,
    STAR,
    EQUAL,
    BANG,
    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS_EQUAL,
    GREATER_EQUAL,
    LESS,
    GREATER,
    SLASH,
    DOT,
    STRING,
    NUMBER,
    INVALID_TOKEN,
    EOF,
};
pub fn printToken(token: Token) !void {
    const token_type = switch (token.token_type) {
        TokenType.EOF => "EOF",
        TokenType.LEFT_PAREN => "LEFT_PAREN",
        TokenType.RIGHT_PAREN => "RIGHT_PAREN",
        TokenType.LEFT_BRACE => "LEFT_BRACE",
        TokenType.RIGHT_BRACE => "RIGHT_BRACE",
        TokenType.SEMICOLON => "SEMICOLON",
        TokenType.COMMA => "COMMA",
        TokenType.PLUS => "PLUS",
        TokenType.MINUS => "MINUS",
        TokenType.STAR => "STAR",
        TokenType.EQUAL => "EQUAL",
        TokenType.BANG => "BANG",
        TokenType.BANG_EQUAL => "BANG_EQUAL",
        TokenType.EQUAL_EQUAL => "EQUAL_EQUAL",
        TokenType.LESS_EQUAL => "LESS_EQUAL",
        TokenType.GREATER_EQUAL => "GREATER_EQUAL",
        TokenType.LESS => "LESS",
        TokenType.GREATER => "GREATER",
        TokenType.SLASH => "SLASH",
        TokenType.DOT => "DOT",
        TokenType.STRING => "STRING",
        TokenType.NUMBER => "NUMBER",
        TokenType.INVALID_TOKEN => "INVALID_TOKEN",
    };
    if (token.literal) |literal| {
        if (token.token_type == TokenType.INVALID_TOKEN) {
            try std.io.getStdErr().writer().print("[line {d}] Error: Unexpected character: {s}\n", .{ literal.number, token.lexeme });
            return error.UnexpectedCharacter;
        } else {
            switch (literal) {
                .number => |number| try std.io.getStdOut().writer().print("{s} {s} {d}\n", .{ token_type, token.lexeme, number }),
                .string => |string| try std.io.getStdOut().writer().print("{s} {s} {s}\n", .{ token_type, token.lexeme, string }),
            }
        }
    } else {
        try std.io.getStdOut().writer().print("{s} {s} null\n", .{ token_type, token.lexeme });
    }
}
pub fn Tokenizer(source: *Input) ![]Token {
    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    var line_number: usize = 1;
    while (source.next()) |c| {
        const token: ?Token = switch (c) {
            ' ', '\t' => continue,
            '\n' => {
                line_number += 1;
                continue;
            },
            '(' => Token{ .token_type = TokenType.LEFT_PAREN, .lexeme = "(", .literal = null },
            ')' => Token{ .token_type = TokenType.RIGHT_PAREN, .lexeme = ")", .literal = null },
            '{' => Token{ .token_type = TokenType.LEFT_BRACE, .lexeme = "{", .literal = null },
            '}' => Token{ .token_type = TokenType.RIGHT_BRACE, .lexeme = "}", .literal = null },
            ';' => Token{ .token_type = TokenType.SEMICOLON, .lexeme = ";", .literal = null },
            ',' => Token{ .token_type = TokenType.COMMA, .lexeme = ",", .literal = null },
            '+' => Token{ .token_type = TokenType.PLUS, .lexeme = "+", .literal = null },
            '-' => Token{ .token_type = TokenType.MINUS, .lexeme = "-", .literal = null },
            '*' => Token{ .token_type = TokenType.STAR, .lexeme = "*", .literal = null },
            '.' => Token{ .token_type = TokenType.DOT, .lexeme = ".", .literal = null },
            '"' => try readString(source),
            '0'...'9' => try readNumber(source, c),
            '=' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '=') {
                        _ = source.next() orelse return error.uhoh;
                        break :switchReturn Token{ .token_type = TokenType.EQUAL_EQUAL, .lexeme = "==", .literal = null };
                    } else {
                        break :switchReturn Token{ .token_type = TokenType.EQUAL, .lexeme = "=", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .token_type = TokenType.EQUAL, .lexeme = "=", .literal = null };
                }
            },
            '!' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '=') {
                        _ = source.next() orelse return error.uhoh;
                        break :switchReturn Token{ .token_type = TokenType.BANG_EQUAL, .lexeme = "!=", .literal = null };
                    } else {
                        break :switchReturn Token{ .token_type = TokenType.BANG, .lexeme = "!", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .token_type = TokenType.BANG, .lexeme = "!", .literal = null };
                }
            },
            '<' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '=') {
                        _ = source.next() orelse return error.uhoh;
                        break :switchReturn Token{ .token_type = TokenType.LESS_EQUAL, .lexeme = "<=", .literal = null };
                    } else {
                        break :switchReturn Token{ .token_type = TokenType.LESS, .lexeme = "<", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .token_type = TokenType.LESS, .lexeme = "<", .literal = null };
                }
            },
            '>' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '=') {
                        _ = source.next() orelse return error.uhoh;
                        break :switchReturn Token{ .token_type = TokenType.GREATER_EQUAL, .lexeme = ">=", .literal = null };
                    } else {
                        break :switchReturn Token{ .token_type = TokenType.GREATER, .lexeme = ">", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .token_type = TokenType.GREATER, .lexeme = ">", .literal = null };
                }
            },
            '/' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '/') {
                        _ = source.next() orelse return error.uhoh;
                        while (source.next()) |cSkip| {
                            if (cSkip == '\n') {
                                line_number += 1;
                                break :switchReturn null;
                            }
                        } else {
                            break :switchReturn null;
                        }
                    } else {
                        break :switchReturn Token{ .token_type = TokenType.SLASH, .lexeme = "/", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .token_type = TokenType.SLASH, .lexeme = "/", .literal = null };
                }
            },
            else => Token{ .token_type = TokenType.INVALID_TOKEN, .lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{c}", .{c}), .literal = Literal{ .number = line_number } },
        };
        if (token) |_| {
            try tokens.append(token.?);
        }
    }
    const token = Token{ .token_type = TokenType.EOF, .lexeme = "", .literal = null };
    try tokens.append(token);

    return tokens.toOwnedSlice();
}

fn readString(source: *Input) !Token {
    var string = std.ArrayList(u8).init(std.heap.page_allocator);
    while (source.next()) |c| {
        if (c == '"') {
            break;
        } else {
            try string.append(c);
        }
    }
    const literal = try string.toOwnedSlice();
    const lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\"", .{literal});

    const token = Token{ .token_type = TokenType.STRING, .lexeme = lexeme, .literal = Literal{ .string = literal } };
    return token;
}

fn readNumber(source: *Input, current: u8) !Token {
    var number = std.ArrayList(u8).init(std.heap.page_allocator);
    try number.append(current);
    while (source.next()) |c| {
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            break;
        } else if (c == '.') {
            try number.append(c);
            break;
        } else if (c >= '0' and c <= '9') {
            try number.append(c);
        } else {
            break;
        }
    }
    if (number.items.len == 0) {
        return error.NumberExpected;
    }
    const literal = try number.toOwnedSlice();
    const lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{literal});
    const token = Token{ .token_type = TokenType.NUMBER, .lexeme = lexeme, .literal = Literal{ .number = std.fmt.parseInt(usize, literal, 10) catch unreachable } };
    return token;
}
