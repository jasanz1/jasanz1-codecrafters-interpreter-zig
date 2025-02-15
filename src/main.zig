const std = @import("std");
const lexer = @import("lexer.zig");
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

    if (!std.mem.eql(u8, command, "tokenize")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);
    var errored: ?anyerror = null;

    // Uncomment this block to pass the first stage
    if (file_contents.len > 0) {
        var input = lexer.makeInput(file_contents);
        const tokens = try lexer.Tokenizer(&input);
        defer std.heap.page_allocator.free(tokens);
        for (tokens) |token| {
            if (lexer.printToken(token)) |_| {} else |err| {
                errored = err;
            }
        }
    } else {
        try std.io.getStdOut().writer().print("EOF  null\n", .{}); // Placeholder, remove this line when implementing the scanner
    }
    if (errored) |err| {
        if (err == error.UnexpectedCharacter or err == error.UnterminatedString or err == error.NumberExpected) {
            return 65;
        }
    }
    return 0;
}
