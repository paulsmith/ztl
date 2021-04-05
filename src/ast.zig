const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Expression = union(enum) {
    number: []const u8,
    ident: []const u8,
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

    // constructors for the variants

    pub fn number(allocator: *Allocator, n: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .number = n };
        return expr;
    }

    pub fn ident(allocator: *Allocator, id: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .ident = id };
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
        id: []const u8,
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

    // constructors for the variants

    pub fn output(allocator: *Allocator, text: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .output = .{ .text = text } };
        return stmt;
    }

    pub fn block(allocator: *Allocator, id: []const u8, body: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .block = .{ .id = id, .body = body } };
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
