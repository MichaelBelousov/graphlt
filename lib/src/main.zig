const std = @import("std");
const builtin = @import("builtin");

pub const Env = @import("./nodes/builtin.zig").Env;
pub const Point = @import("./nodes/builtin.zig").Point;
pub const Pin = @import("./nodes/builtin.zig").Pin;
pub const PrimitivePin = @import("./nodes/builtin.zig").PrimitivePin;
pub const primitive_types = @import("./nodes/builtin.zig").primitive_types;
pub const Type = @import("./nodes/builtin.zig").Type;
pub const TypeInfo = @import("./nodes/builtin.zig").TypeInfo;
pub const NodeDesc = @import("./nodes/builtin.zig").NodeDesc;
pub const BasicNodeDesc = @import("./nodes/builtin.zig").BasicNodeDesc;
pub const helpers = @import("./nodes/builtin.zig");
pub const Value = @import("./nodes/builtin.zig").Value;
pub const Sexp = @import("./sexp.zig").Sexp;
pub const syms = @import("./sexp.zig").syms;
pub const SexpParser = @import("./sexp_parser.zig").Parser;
pub const compiler = @import("./compiler-wat.zig");

pub const testing = struct {
    pub const expectWasmOutput = compiler.expectWasmOutput;
};

test {
    // FIXME:
    std.testing.refAllDeclsRecursive(compiler);
    std.testing.refAllDeclsRecursive(@import("./sexp_parser.zig"));
    //std.testing.refAllDeclsRecursive(@This());
}
