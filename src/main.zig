const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const eval = @import("eval.zig");

pub fn main() !u8 {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize") and !std.mem.eql(u8, command, "parse") and !std.mem.eql(u8, command, "evaluate")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    // Uncomment this block to pass the first stage
    var tokenizerInput = lexer.Input{ .source = file_contents };
    const tokens = lexer.lexer(&tokenizerInput, std.mem.eql(u8, command, "tokenize")) catch return 65;
    defer std.heap.page_allocator.free(tokens);
    if (std.mem.eql(u8, command, "tokenize")) {
        try lexer.printTokens(tokens);
        lexer.errorCheck(tokens) catch return 65;
        return 0;
    }

    var pasterInput = parser.Input{ .source = tokens };
    const ast = parser.parser(&pasterInput, std.mem.eql(u8, command, "parse")) catch return 65;
    if (std.mem.eql(u8, command, "parse")) {
        parser.errorCheck(&ast) catch return 65;
        try parser.printExpression(std.io.getStdOut().writer(), &ast);
        return 0;
    }

    const value = eval.evalulate(&ast) catch return 70;
    if (std.mem.eql(u8, command, "evaluate")) {
        try eval.printValue(std.io.getStdOut().writer(), &value);
        return 0;
    }
    return 0;
}
