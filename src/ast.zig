const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Expression = union(enum) {
    number: []const u8,
    name: []const u8,
    string: []const u8,
    unary_op: struct {
        op: []const u8,
        expr: *Expression,
    },
    bin_op: struct {
        op: []const u8,
        lhs: *Expression,
        rhs: *Expression,
    },

    const Self = @This();

    pub fn destroy(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .unary_op => |val| {
                val.expr.destroy(allocator);
            },
            .bin_op => |val| {
                val.lhs.destroy(allocator);
                val.rhs.destroy(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn formatter(self: Self) ExpressionFormatter {
        return ExpressionFormatter{ .expr = self };
    }

    // constructors for the variants

    pub fn number(allocator: *Allocator, n: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .number = n };
        return expr;
    }

    pub fn name(allocator: *Allocator, id: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .name = id };
        return expr;
    }

    pub fn string(allocator: *Allocator, str: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .string = str };
        return expr;
    }

    pub fn unaryOp(allocator: *Allocator, op: []const u8, sub_expr: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .unary_op = .{ .op = op, .expr = sub_expr } };
        return expr;
    }

    pub fn binOp(allocator: *Allocator, op: []const u8, lhs: *Expression, rhs: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .bin_op = .{ .op = op, .lhs = lhs, .rhs = rhs } };
        return expr;
    }
};

const ExpressionFormatter = struct {
    expr: Expression,

    const Self = @This();

    // NOTE(paulsmith): see comment above .format() on the StatementFormatter below.
    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        var buf: [1024]u8 = undefined;
        var allocator = &std.heap.FixedBufferAllocator.init(buf[0..]).allocator;

        const Item = struct {
            expr: *const Expression,
            append_sep: bool,
        };

        var empty = std.ArrayList(Item).init(allocator);
        var rest = std.ArrayList(Item).init(allocator);

        empty.append(.{ .expr = &self.expr, .append_sep = false }) catch @panic("couldn't append to the empty stack");

        while (empty.popOrNull()) |item| {
            rest.append(item) catch @panic("couldn't append to the rest stack");

            switch (item.expr.*) {
                .unary_op => |unary_op| {
                    try writer.writeAll("(");
                    try writer.writeAll(unary_op.op);
                    try writer.writeAll(" ");
                    empty.append(.{ .expr = unary_op.expr, .append_sep = false }) catch @panic("couldn't append to the empty stack");
                },
                .bin_op => |bin_op| {
                    try writer.writeAll("(");
                    try writer.writeAll(bin_op.op);
                    try writer.writeAll(" ");
                    empty.append(.{ .expr = bin_op.lhs, .append_sep = true }) catch @panic("couldn't append to the empty stack");
                    empty.append(.{ .expr = bin_op.rhs, .append_sep = false }) catch @panic("couldn't append to the empty stack");
                },
                else => {},
            }
        }

        var prepend_sep = false;
        while (rest.popOrNull()) |item| {
            switch (item.expr.*) {
                .name, .string, .number => |str| {
                    if (prepend_sep) try writer.writeAll(" ");
                    prepend_sep = false;
                    try writer.writeAll(str);
                    if (item.append_sep) try writer.writeAll(" ");
                },
                .bin_op, .unary_op => {
                    try writer.writeAll(")");
                    prepend_sep = true;
                },
            }
        }
    }
};

pub const Statement = union(enum) {
    output: struct {
        text: []const u8,
    },
    block: struct {
        name: []const u8,
        body: []*Statement,
    },
    extends: struct {
        filename: []const u8,
    },
    @"for": struct {
        element: []const u8,
        collection: *Expression,
        body: []*Statement,
    },
    @"if": struct {
        predicate: *Expression,
        consequent: []*Statement,
    },
    expr: *Expression,

    const Self = @This();

    pub fn destroy(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .block => |val| {
                for (val.body) |stmt| stmt.destroy(allocator);
                allocator.free(val.body);
            },
            .@"for" => |val| {
                val.collection.destroy(allocator);
                for (val.body) |stmt| stmt.destroy(allocator);
                allocator.free(val.body);
            },
            .@"if" => |val| {
                val.predicate.destroy(allocator);
                for (val.consequent) |stmt| stmt.destroy(allocator);
                allocator.free(val.consequent);
            },
            .expr => |val| val.destroy(allocator),
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn formatter(self: Self) StatementFormatter {
        return StatementFormatter{ .stmt = self };
    }

    // constructors for the variants

    pub fn output(allocator: *Allocator, text: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .output = .{ .text = text } };
        return stmt;
    }

    pub fn block(allocator: *Allocator, name: []const u8, body: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .block = .{ .name = name, .body = body } };
        return stmt;
    }

    pub fn extends(allocator: *Allocator, filename: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .extends = .{ .filename = filename } };
        return stmt;
    }

    pub fn @"for"(allocator: *Allocator, element: []const u8, collection: *Expression, body: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .@"for" = .{ .element = element, .collection = collection, .body = body } };
        return stmt;
    }

    pub fn @"if"(allocator: *Allocator, predicate: *Expression, consequent: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .@"if" = .{ .predicate = predicate, .consequent = consequent } };
        return stmt;
    }

    pub fn expression(allocator: *Allocator, expr: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .expr = expr };
        return stmt;
    }
};

// NOTE(paulsmith): this is a workaround due to a Zig compiler bug that causes an infinite loop in the Semantic Analysis step when there is a recursive call in the format() function, adhering to the interface described in the std.fmt.format comment. We wrap the AST object in a formatter type (retrieved with a call to .formatter()) that builds a string representation of the object iteratively, side-stepping the recursive issue. Once this compiler bug [https://github.com/ziglang/zig/issues/4572] is fixed, the format() function can be implemented directly on the AST type and can use the recursive implementation.
const StatementFormatter = struct {
    stmt: Statement,

    const Self = @This();

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        var buf: [1024]u8 = undefined; // NOTE(paulsmith): hopefully this is enough space for a large AST but not so large that it blows the stack. Might have to take an allocator from the caller if that becomes a problem, changing the API.
        var allocator = &std.heap.FixedBufferAllocator.init(buf[0..]).allocator;

        const Item = struct {
            stmt: *const Statement,
            append_sep: bool,
        };

        var empty = std.ArrayList(Item).init(allocator);
        var rest = std.ArrayList(Item).init(allocator);

        empty.append(.{ .stmt = &self.stmt, .append_sep = false }) catch @panic("couldn't append to the empty stack");

        while (empty.popOrNull()) |item| {
            rest.append(item) catch @panic("couldn't append to the rest stack");
            switch (item.stmt.*) {
                .block => |block| {
                    try writer.writeAll("(Block (name ");
                    try writer.writeAll(block.name);
                    try writer.writeAll(") (body ");
                    for (block.body) |stmt, i| {
                        const append_sep = i < block.body.len - 1;
                        empty.append(.{ .stmt = stmt, .append_sep = append_sep }) catch @panic("couldn't append to the empty stack");
                    }
                },
                .@"for" => |@"for"| {
                    try writer.writeAll("(For (element ");
                    try writer.writeAll(@"for".element);
                    try writer.writeAll(") (collection ");
                    try std.fmt.format(writer, "{}", .{@"for".collection.formatter()});
                    try writer.writeAll(") (body ");
                    for (@"for".body) |stmt, i| {
                        const append_sep = i < @"for".body.len - 1;
                        empty.append(.{ .stmt = stmt, .append_sep = append_sep }) catch @panic("couldn't append to the empty stack");
                    }
                },
                .@"if" => |@"if"| {
                    try writer.writeAll("(If (predicate ");
                    try std.fmt.format(writer, "{}", .{@"if".predicate.formatter()});
                    try writer.writeAll(") (consequent ");
                    for (@"if".consequent) |stmt, i| {
                        const append_sep = i < @"if".consequent.len - 1;
                        empty.append(.{ .stmt = stmt, .append_sep = append_sep }) catch @panic("couldn't append to the empty stack");
                    }
                },
                else => {},
            }
        }

        while (rest.popOrNull()) |item| {
            switch (item.stmt.*) {
                .block, .@"for", .@"if" => try writer.writeAll("))"),
                .output => |output| {
                    const max_len = 10;
                    var snippet: [2 * max_len]u8 = undefined;
                    const len = std.math.min(output.text.len, max_len);
                    const dots = if (len < output.text.len) "..." else "";
                    const replacements = std.mem.replace(u8, output.text[0..len], "\n", "\\n", snippet[0..]);
                    try writer.writeAll("(Output (text \"");
                    try writer.writeAll(snippet[0 .. len + replacements]);
                    try writer.writeAll(dots);
                    try writer.writeAll("\"))");
                },
                .extends => |extends| {
                    try writer.writeAll("(Extends (filename \"");
                    try writer.writeAll(extends.filename);
                    try writer.writeAll("\"))");
                },
                .expr => |expr| {
                    try writer.writeAll("(Expr ");
                    try std.fmt.format(writer, "{}", .{expr.formatter()});
                    try writer.writeAll(")");
                },
            }
            if (item.append_sep) try writer.writeAll(" ");
        }
    }
};

pub const Tree = struct {
    stmts: []*Statement,
    allocator: *Allocator,

    const Self = @This();

    pub fn destroy(self: *Self) void {
        for (self.stmts) |stmt| {
            stmt.destroy(self.allocator);
        }
        self.allocator.free(self.stmts);
    }
};
