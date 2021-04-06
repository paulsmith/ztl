const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Expression = union(enum) {
    number: []const u8,
    name: []const u8,
    string: []const u8,
    bin_op: struct {
        op: []const u8,
        lhs: *Expression,
        rhs: *Expression,
    },

    const Self = @This();

    pub fn destroy(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .bin_op => |val| {
                val.lhs.destroy(allocator);
                val.rhs.destroy(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .number => {
                try std.fmt.format(writer, "number={}", .{self.number});
            },
            .name => {
                try std.fmt.format(writer, "name={}", .{self.name});
            },
            .string => {
                try std.fmt.format(writer, "string=\"{}\"", .{self.string});
            },
            .bin_op => {
                try std.fmt.format(writer, "(", .{});
                try self.bin_op.lhs.format(fmt, options, writer);
                try std.fmt.format(writer, " {} ", .{self.bin_op.op});
                try self.bin_op.rhs.format(fmt, options, writer);
                try std.fmt.format(writer, ")", .{});
            },
        }
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

    pub fn binOp(allocator: *Allocator, op: []const u8, lhs: *Expression, rhs: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .bin_op = .{ .op = op, .lhs = lhs, .rhs = rhs } };
        return expr;
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

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .output => {
                var snippet: [20]u8 = undefined;
                const len = std.math.min(self.output.text.len, 10);
                const dots = if (len < self.output.text.len) "..." else "";
                _ = std.mem.replace(u8, self.output.text[0..len], "\n", "\\n", snippet[0..]);
                try std.fmt.format(writer, "Output[text=\"{}{}\"]", .{ snippet, dots });
            },
            .block => {
                try std.fmt.format(writer, "Block[name={} ", .{self.block.name});
                for (self.block.body) |stmt, i| {
                    try stmt.format(fmt, options, writer);
                    if (i < self.block.body.len - 1) try std.fmt.format(writer, ", ", .{});
                }
                try std.fmt.format(writer, "]", .{});
            },
            .extends => {
                try std.fmt.format(writer, "Extends[filename=\"{}\"]", .{self.extends.filename});
            },
            .@"for" => {
                try std.fmt.format(writer, "For[collection=", .{});
                try self.@"for".collection.format(fmt, options, writer);
                try std.fmt.format(writer, " body=[", .{});
                for (self.@"for".body) |stmt, i| {
                    try stmt.format(fmt, options, writer);
                    if (i < self.@"for".body.len - 1) try std.fmt.format(writer, ", ", .{});
                }
                try std.fmt.format(writer, "]]", .{});
            },
            .@"if" => {
                try std.fmt.format(writer, "If[predicate=", .{});
                try self.@"if".predicate.format(fmt, options, writer);
                try std.fmt.format(writer, " consequent=[", .{});
                for (self.@"if".consequent) |stmt, i| {
                    try stmt.format(fmt, options, writer);
                    if (i < self.@"if".consequent.len - 1) try std.fmt.format(writer, ", ", .{});
                }
                try std.fmt.format(writer, "]]", .{});
            },
            .expr => {
                try std.fmt.format(writer, "Expr[", .{});
                try self.expr.format(fmt, options, writer);
                try std.fmt.format(writer, "]", .{});
            },
        }
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
