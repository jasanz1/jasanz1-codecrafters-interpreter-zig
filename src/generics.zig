/// A generic input struct that can be used to read from a slice of any type.
/// This function takes a comptime value_type and returns a function that takes a value_type and returns a struct.
/// The struct has a source field of type value_type, an index field of type usize, and a line_number field of type usize.
/// The next function returns the next value_type in the source slice, or null if there are no more values.
/// The peek function returns the next value_type in the source slice without advancing the index, or null if there are no more values.
pub fn makeInput(comptime value_type: type) type {
    return struct {
        source: []const value_type,
        index: usize = 0,
        line_number: usize = 1,
        const Self = @This();
        pub fn next(self: *Self) ?value_type {
            if (self.index >= self.source.len) {
                return null;
            }
            defer self.index += 1;
            return self.source[self.index];
        }
        pub fn peek(self: *Self) ?value_type {
            if (self.index >= self.source.len) {
                return null;
            }
            return self.source[self.index];
        }
    };
}
