const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("zig-templates", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    var lex_tests = b.addTest("src/lex.zig");
    lex_tests.setBuildMode(mode);
    var parser_tests = b.addTest("src/parser.zig");
    parser_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&lex_tests.step);
    test_step.dependOn(&parser_tests.step);
}
