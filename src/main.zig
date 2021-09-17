const std = @import("std");
const Template = @import("./glitz.zig").Template;

pub fn main() !void {
    std.debug.print("greetings, world\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;

    const templates = try Template.load(allocator, "./templates");
    defer templates.deinit();
    const template = templates.get("index.html").?;
    const Story = struct {
        title: []const u8,
        url: []const u8,
        tease: []const u8,
    };
    const story_list = [_]Story{
        .{ .title = "Hello, templates!", .url = "https://pauladamsmith.com/", .tease = "Now is the time for all good men to come to the aid of their party." },
        .{ .title = "Moar title", .url = "https://web.archive.org/", .tease = "The <i>quick brown fox</i> jumps over the lazy dog." },
    };
    const output = try template.render(allocator, .{ .title = "Hello, world!", .story_list = story_list });
    std.debug.print("RENDERED OUTPUT:\n{s}\n", .{output});
}
