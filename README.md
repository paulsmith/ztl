Templates for Zig based on Python's Jinja and Django.

Example template

<ul>
{% for item in mylist %}
	<li><a href="{{ item.url }}">{{ item.name }}</a></li>
{% endfor %}
</ul>
<p>{{ a_variable }}</p>
{# a comment #}

Example usage from Zig

const template = @import("template");
const std = @import("std");

const Item = struct {
	url: []u8,
	name: []u8,
};

const my_list = [_]Item{
	.{.url = "https://ziglang.org/", .name = "Zig Programming Language"}, 
	.{.url = "https://ziglearn.org/", .name = "ziglearn.org"}, 
};

const t = try template.parseFile("mytemplate.html");
const output = try t.render(.{mylist = my_list, a_variable = @as(u32, 1234)});
std.debug.log("{s}\n", .{output});

Resources / reference

SerenityOS HTML parser

https://github.com/SerenityOS/serenity/blob/master/Userland/Libraries/LibWeb/HTML/Parser/HTMLTokenizer.h

Bitwise ion parser

https://github.com/pervognsen/bitwise/blob/master/ion/lex.c

HTML parsing spec

https://html.spec.whatwg.org/multipage/parsing.html#tokenization

Jinja

https://palletsprojects.com/p/jinja/
https://jinja.palletsprojects.com/en/2.11.x/templates/
https://github.com/pallets/jinja/blob/master/src/jinja2/lexer.py

Django

https://docs.djangoproject.com/en/3.1/ref/templates/language/

Zig

https://github.com/ziglang/zig/blob/master/src/DepTokenizer.zig
https://github.com/ziglang/zig/blob/master/lib/std/zig/tokenizer.zig

Go

https://golang.org/src/text/template/parse/lex.go // switch to function pointers for state

Tera

https://tera.netlify.app/docs/

Nunjucks

https://mozilla.github.io/nunjucks/
