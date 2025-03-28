//! This is the lexer for the interpreter
//! It takes a string and turns it into an array of tokens
const std = @import("std");

/// takes a u8 array and turns into a lexer input
/// This is used to tokenize the input.
pub const Input = @import("generics.zig").makeInput(u8);

/// A token struct that contains the lexeme, type, and literal value of a token.
pub const Token = struct {
    line_number: usize,
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
};

/// A union of all possible literal values.
pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
};

/// A union of all possible token types.
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
    IDENTIFIER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    INVALID_TOKEN,
    UNTERMINATED_STRING,
    EOF,
};

/// prints a given token
pub fn printToken(token: Token) !void {
    const token_type = @tagName(token.token_type);
    if (token.literal) |literal| {
        switch (literal) {
            .number => |number| {
                //dont know of a clean way to set min precision
                if (@ceil(number) == number) {
                    try std.io.getStdOut().writer().print("{s} {s} {d}.0\n", .{ token_type, token.lexeme, number });
                    return;
                } else {
                    try std.io.getStdOut().writer().print("{s} {s} {d}\n", .{ token_type, token.lexeme, number });
                    return;
                }
            },
            .string => |string| try std.io.getStdOut().writer().print("{s} {s} {s}\n", .{ token_type, token.lexeme, string }),
        }
    } else {
        switch (token.token_type) {
            TokenType.INVALID_TOKEN => {
                try std.io.getStdErr().writer().print("[line {d}] Error: Unexpected character: {s}\n", .{ token.line_number, token.lexeme });
                return;
            },
            TokenType.UNTERMINATED_STRING => {
                try std.io.getStdErr().writer().print("[line {d}] Error: Unterminated string.\n", .{token.line_number});
                return;
            },
            else => {
                try std.io.getStdOut().writer().print("{s} {s} null\n", .{ token_type, token.lexeme });
                return;
            },
        }
    }
}

///prints all tokens in a given token array
pub fn printTokens(tokens: []Token) !void {
    for (tokens) |token| {
        try printToken(token);
    }
    return;
}

/// checks for lexer errors
pub fn errorCheck(tokens: []Token) !void {
    for (tokens) |token| {
        switch (token.token_type) {
            TokenType.INVALID_TOKEN => {
                return error.UnexpectedCharacter;
            },
            TokenType.UNTERMINATED_STRING => {
                return error.UnterminatedString;
            },
            else => {},
        }
    }
}

/// main entry point for the lexer. takes a lexer input turns it into a array of tokens
pub fn lexer(source: *Input, ignore_errors: bool) ![]Token {
    const tokens = try Tokenizer(source);
    if (!ignore_errors) {
        try errorCheck(tokens);
    }
    return tokens;
}

/// main function for the lexer, walks the input and turns ever char or string of char into a token
/// makes a map of keywords and assigns them to their respective token type so they can be differentiated from identifiers
/// uses \n to determine line numbers
fn Tokenizer(source: *Input) ![]Token {
    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    var keyword_map = std.StringHashMap(TokenType).init(std.heap.page_allocator);
    try keyword_map.put("and", TokenType.AND);
    try keyword_map.put("class", TokenType.CLASS);
    try keyword_map.put("else", TokenType.ELSE);
    try keyword_map.put("false", TokenType.FALSE);
    try keyword_map.put("for", TokenType.FOR);
    try keyword_map.put("fun", TokenType.FUN);
    try keyword_map.put("if", TokenType.IF);
    try keyword_map.put("nil", TokenType.NIL);
    try keyword_map.put("or", TokenType.OR);
    try keyword_map.put("print", TokenType.PRINT);
    try keyword_map.put("return", TokenType.RETURN);
    try keyword_map.put("super", TokenType.SUPER);
    try keyword_map.put("this", TokenType.THIS);
    try keyword_map.put("true", TokenType.TRUE);
    try keyword_map.put("var", TokenType.VAR);
    try keyword_map.put("while", TokenType.WHILE);
    while (source.next()) |c| {
        const token: ?Token = switch (c) {
            ' ', '\t' => continue,
            '\n' => {
                source.line_number += 1;
                continue;
            },
            '(' => Token{ .line_number = source.line_number, .token_type = TokenType.LEFT_PAREN, .lexeme = "(", .literal = null },
            ')' => Token{ .line_number = source.line_number, .token_type = TokenType.RIGHT_PAREN, .lexeme = ")", .literal = null },
            '{' => Token{ .line_number = source.line_number, .token_type = TokenType.LEFT_BRACE, .lexeme = "{", .literal = null },
            '}' => Token{ .line_number = source.line_number, .token_type = TokenType.RIGHT_BRACE, .lexeme = "}", .literal = null },
            ';' => Token{ .line_number = source.line_number, .token_type = TokenType.SEMICOLON, .lexeme = ";", .literal = null },
            ',' => Token{ .line_number = source.line_number, .token_type = TokenType.COMMA, .lexeme = ",", .literal = null },
            '+' => Token{ .line_number = source.line_number, .token_type = TokenType.PLUS, .lexeme = "+", .literal = null },
            '-' => Token{ .line_number = source.line_number, .token_type = TokenType.MINUS, .lexeme = "-", .literal = null },
            '*' => Token{ .line_number = source.line_number, .token_type = TokenType.STAR, .lexeme = "*", .literal = null },
            '.' => Token{ .line_number = source.line_number, .token_type = TokenType.DOT, .lexeme = ".", .literal = null },
            '"' => try readString(source),
            '0'...'9' => try readNumber(source, c),
            '=' => try readmultiCharacterEqualToken(source, TokenType.EQUAL, TokenType.EQUAL_EQUAL, c),
            '!' => try readmultiCharacterEqualToken(source, TokenType.BANG, TokenType.BANG_EQUAL, c),
            '<' => try readmultiCharacterEqualToken(source, TokenType.LESS, TokenType.LESS_EQUAL, c),
            '>' => try readmultiCharacterEqualToken(source, TokenType.GREATER, TokenType.GREATER_EQUAL, c),
            '/' => switchReturn: {
                if (source.peek()) |cPeek| {
                    if (cPeek == '/') {
                        _ = source.next() orelse return error.uhoh;
                        while (source.next()) |cSkip| {
                            if (cSkip == '\n') {
                                source.line_number += 1;
                                break :switchReturn null;
                            }
                        } else {
                            break :switchReturn null;
                        }
                    } else {
                        break :switchReturn Token{ .line_number = source.line_number, .token_type = TokenType.SLASH, .lexeme = "/", .literal = null };
                    }
                } else {
                    break :switchReturn Token{ .line_number = source.line_number, .token_type = TokenType.SLASH, .lexeme = "/", .literal = null };
                }
            },
            else => switchReturn: {
                const identifier = try readIdentifer(source, c);

                if (keyword_map.get(identifier.lexeme)) |keyword| {
                    break :switchReturn Token{ .line_number = source.line_number, .token_type = keyword, .lexeme = identifier.lexeme, .literal = null };
                }
                break :switchReturn identifier;
            },
        };
        if (token) |_| {
            try tokens.append(token.?);
        }
    }
    const token = Token{ .line_number = source.line_number, .token_type = TokenType.EOF, .lexeme = "", .literal = null };
    try tokens.append(token);
    const finalTokens = tokens.toOwnedSlice() catch @panic("error allocating memory for tokens");
    return finalTokens;
}

/// reads an identifer from the input, if it is not a valid identifer it returns an error
/// makes sure the first character is a valid character for an identifier
/// then until it hits a non alphanumeric character or a newline it appends the character to the identifier
fn readIdentifer(source: *Input, current: u8) !Token {
    var identifier = std.ArrayList(u8).init(std.heap.page_allocator);
    if (current != '_' and !std.ascii.isAlphanumeric(current)) {
        return Token{ .line_number = source.line_number, .token_type = TokenType.INVALID_TOKEN, .lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{c}", .{current}), .literal = null };
    }
    try identifier.append(current);
    while (source.peek()) |c| {
        switch (c) {
            '\n', '\r' => {
                source.line_number += 1;
                break;
            },
            'A'...'Z', 'a'...'z', '0'...'9', '_' => {
                _ = source.next();
                try identifier.append(c);
            },
            else => {
                break;
            },
        }
    }
    const lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{identifier.items});
    const token = Token{ .line_number = source.line_number, .token_type = TokenType.IDENTIFIER, .lexeme = lexeme, .literal = null };
    return token;
}

/// reads a string from the input, if it is not a valid string it returns an error
/// takes chars until it hits a " or \r
/// if it hits a \r it increments the line number and returns an unterminated string error
/// if it hits a " it returns a token with the string as the lexeme
fn readString(source: *Input) !Token {
    var string = std.ArrayList(u8).init(std.heap.page_allocator);
    while (source.peek()) |c| {
        if (c == '"') {
            _ = source.next();
            break;
        } else if (c == '\r') {
            source.line_number += 1;
            return Token{ .line_number = source.line_number, .token_type = TokenType.UNTERMINATED_STRING, .lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\"", .{string.items}), .literal = null };
        } else {
            _ = source.next();
            try string.append(c);
        }
    } else {
        return Token{ .line_number = source.line_number, .token_type = TokenType.UNTERMINATED_STRING, .lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\"", .{string.items}), .literal = null };
    }
    const literal = try string.toOwnedSlice();
    const lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\"", .{literal});

    const token = Token{ .line_number = source.line_number, .token_type = TokenType.STRING, .lexeme = lexeme, .literal = Literal{ .string = literal } };
    return token;
}

/// reads a number from the input, if it is not a valid number it returns an error
/// takes chars until it hits a non number character or a newline
/// if it hits a non number character it returns a token with the number as the lexeme
/// makes sure number arrays are not empty then parses the number
fn readNumber(source: *Input, current: u8) !Token {
    var number = std.ArrayList(u8).init(std.heap.page_allocator);
    try number.append(current);
    while (source.peek()) |c| {
        if (c == '\n' or c == '\r') {
            source.line_number += 1;
            break;
        } else if (c == ' ' or c == '\t') {
            break;
        } else if (c == '.') {
            _ = source.next();
            try number.append(c);
        } else if (c >= '0' and c <= '9') {
            _ = source.next();
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
    const token = Token{ .line_number = source.line_number, .token_type = TokenType.NUMBER, .lexeme = lexeme, .literal = Literal{ .number = std.fmt.parseFloat(f64, literal) catch unreachable } };
    return token;
}

/// takes current char and peeks next char, if the next char is an = then takes it and turns it into multi_token_type otherwise turns current char into single_token_type
fn readmultiCharacterEqualToken(source: *Input, single_token_type: TokenType, multi_token_type: TokenType, current: u8) !Token {
    const current_str = try std.fmt.allocPrint(std.heap.page_allocator, "{c}", .{current});
    if (source.peek()) |cPeek| {
        if (cPeek == '=') {
            _ = source.next() orelse return error.uhoh;
            const lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{c}{c}", .{ current, cPeek });
            return Token{ .line_number = source.line_number, .token_type = multi_token_type, .lexeme = lexeme, .literal = null };
        } else {
            return Token{ .line_number = source.line_number, .token_type = single_token_type, .lexeme = current_str, .literal = null };
        }
    } else {
        return Token{ .line_number = source.line_number, .token_type = single_token_type, .lexeme = current_str, .literal = null };
    }
}

test "parserHappy" {
    const TestCases = struct {
        input: []const u8,
        expected_error: anyerror,
    };
    const test_input = [_]TestCases{
        TestCases{ .input = "\"foo\" \"unterminated", .expected_error = error.UnterminatedString },
    };

    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = lexer(&inputTokens, false);
        try std.testing.expectError(test_case.expected_error, tokens);
        std.debug.print("\n\n\n", .{});
    }
}
