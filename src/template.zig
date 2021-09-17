const std = @import("std");
const path = std.fs.path;
const parser = @import("./parser.zig");
const ast = @import("./ast.zig");
const Tree = ast.Tree;

const Template = struct {
    filename: []const u8,
    tree: Tree,
};

pub fn loadFile(allocator: *Allocator, template_path: []const u8) !Template {}
