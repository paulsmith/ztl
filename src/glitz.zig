const std = @import("std");
const parse = @import("./parser.zig").parse;
const ast = @import("./ast.zig");
const Tree = ast.Tree;
const Expression = ast.Expression;

const max_file_size = 2 * 1024 * 1024; // 2 MB arbitrarily

const Error = error{
    InvalidOperandType,
    NameNotFound,
} || std.fmt.ParseIntError;

pub const Environment = struct {
    allocator: *std.mem.Allocator,
    map: std.ArrayList(KV),

    const Self = @This();

    const KV = struct {
        name: []const u8,
        value: Value,
    };

    pub fn create(allocator: *std.mem.Allocator, args: anytype) !*Self {
        var self = try allocator.create(Self);
        self.* = .{
            .allocator = allocator,
            .map = std.ArrayList(KV).init(allocator),
        };
        return self;
    }

    pub fn destroy(self: *Self) void {
        self.map.deinit(self.allocator);
        self.allocator.free(self);
    }

    pub fn lookup(self: *Self, name: []const u8) ?Value {
        for (self.map.items) |kv| if (std.mem.eql(u8, kv.name, name)) return kv.value;
        return null;
    }
};

const Value = union(enum) {
    number: i64,
    string: []const u8,
    @"bool": bool,
    // func call ...
};

fn eval(expr: *Expression, env: *Environment) Error!Value {
    return switch (expr.*) {
        .number => |n| Value{ .number = try std.fmt.parseInt(i64, n, 0) },
        .name => |name| {
            return env.lookup(name) orelse error.NameNotFound;
        },
        .string => |str| Value{ .string = str },
        .unary_op => |unary_op| {
            const operand = try eval(unary_op.expr, env);
            switch (unary_op.op) {
                .minus => {
                    if (operand != .number) return error.InvalidOperandType;
                    return Value{ .number = operand.number * -1 };
                },
                .plus => {
                    if (operand != .number) return error.InvalidOperandType;
                    return Value{ .number = operand.number };
                },
                .not => {
                    if (operand != .@"bool") return error.InvalidOperandType;
                    return Value{ .@"bool" = !operand.@"bool" };
                },
                else => unreachable,
            }
        },
        .bin_op => |bin_op| {
            const lhs = try eval(bin_op.lhs, env);
            const rhs = try eval(bin_op.rhs, env);
            switch (bin_op.op) {
                .plus => {
                    // TODO support concatenating strings
                    return Value{ .number = lhs.number + rhs.number };
                },
                .minus => {
                    return Value{ .number = lhs.number - rhs.number };
                },
                .star => {
                    return Value{ .number = lhs.number * rhs.number };
                },
                .forward_slash => {
                    return Value{ .number = @divTrunc(lhs.number, rhs.number) };
                },
                .less_than => {
                    return Value{ .@"bool" = lhs.number < rhs.number };
                },
                .greater_than => {
                    return Value{ .@"bool" = lhs.number < rhs.number };
                },
                .lt_or_equal_to => {
                    return Value{ .@"bool" = lhs.number <= rhs.number };
                },
                .gt_or_equal_to => {
                    return Value{ .@"bool" = lhs.number >= rhs.number };
                },
                .equal_to => {
                    return Value{ .@"bool" = lhs.number == rhs.number };
                },
                .not_equal => {
                    return Value{ .@"bool" = lhs.number != rhs.number };
                },
                .keyword_and => {
                    return Value{ .@"bool" = lhs.@"bool" and rhs.@"bool" };
                },
                .keyword_or => {
                    return Value{ .@"bool" = lhs.@"bool" or rhs.@"bool" };
                },
                else => std.debug.panic("token kind '{s}' is not an operator", .{bin_op.op}),
            }
        },
        .func_call => |func_call| {
            std.debug.panic("func calls are not implemented yet {}", .{func_call});
        },
    };
}

pub const Template = struct {
    tree: Tree,
    group: ?*TemplateGroup,

    pub fn load(allocator: *std.mem.Allocator, templates_path: []const u8) !*TemplateGroup {
        var group = try allocator.create(TemplateGroup);
        group.* = .{ .map = TemplateHashMap.init(allocator), .allocator = allocator };
        var dir = try std.fs.cwd().openDir(templates_path, .{ .iterate = true });
        defer dir.close();
        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .File) continue;
            const name = entry.name;
            if (std.mem.startsWith(u8, name, ".")) continue;
            std.debug.print("\n*** Parsing {s}\n", .{name});
            const contents = try dir.readFileAlloc(allocator, name, max_file_size);
            const tree = try parse(allocator, name, contents, .{});
            const template = Template{ .tree = tree, .group = group };
            // TODO test for dupe filenames if/when we support sub-directories
            try group.map.put(name, template);
        }
        return group;
    }

    pub fn render(self: Template, allocator: *std.mem.Allocator, args: anytype) ![]u8 {
        //std.debug.print("{} {}\n", .{ allocator, args });
        var env = try Environment.create(allocator, args);
        var buf = std.ArrayList(u8).init(allocator);
        var writer = std.io.getStdOut().writer();
        for (self.tree.stmts) |stmt_ptr| {
            // const value = stmt.*;
            // if (value == .output) {
            //     try writer.writeAll("@@ GOT OUTPUT\n");
            // }
            // const T = @TypeOf(value);
            // std.debug.print("{}", .{@typeName(T)});
            // const info = @typeInfo(T).Union;
            // if (info.tag_type) |UnionTagType| {
            //     try writer.writeAll("{ .");
            //     try writer.writeAll(@tagName(@as(UnionTagType, value)));
            //     try writer.writeAll(" = ");
            //     inline for (info.fields) |u_field| {
            //         if (value == @field(UnionTagType, u_field.name)) {
            //             try std.fmt.formatType(@field(value, u_field.name), "", std.fmt.FormatOptions{}, writer, 0);
            //         }
            //     }
            //     try writer.writeAll(" }");
            // } else {
            //     try format(writer, "@{x}", .{@ptrToInt(&value)});
            // }
            // try writer.writeAll("\n");
            const stmt = stmt_ptr.*;
            switch (stmt) {
                .output => |output| try buf.appendSlice(output.text),
                .block => try buf.appendSlice("UNIMPLEMENTED BLOCK\n"),
                .extends => try buf.appendSlice("UNIMPLEMENTED EXTENDS\n"),
                .expr => |expr| {
                    const val = try eval(expr, env);
                    try std.fmt.format(writer, "{}", .{val});
                },
                .@"for" => {
                    try buf.appendSlice("UNIMPLEMENTED FOR\n");
                },
                .@"if" => {
                    try buf.appendSlice("UNIMPLEMENTED IF\n");
                },
            }
        }
        return buf.toOwnedSlice();
    }
};

const TemplateHashMap = std.StringHashMap(Template);

const TemplateGroup = struct {
    map: TemplateHashMap,
    allocator: *std.mem.Allocator,

    const Self = @This();

    pub fn get(self: *Self, name: []const u8) ?Template {
        return self.map.get(name);
    }

    pub fn deinit(self: *Self) void {
        var it = self.map.iterator();
        while (it.next()) |entry| entry.value_ptr.tree.destroy();
        self.map.deinit();
        self.allocator.destroy(self);
    }
};
