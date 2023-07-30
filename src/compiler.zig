const std = @import("std");

const _chunk = @import("chunk.zig");
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;

const _value = @import("value.zig");
const Value = _value.Value;

const _scanner = @import("scanner.zig");
const TokenType = _scanner.TokenType;
const Token = _scanner.Token;
const Scanner = _scanner.Scanner;

const _object = @import("object.zig");

const _debug = @import("debug.zig");

const debug_print_code = true;

pub fn compile(
    alloc: std.mem.Allocator,
    vm_objects: *?*_value.Obj, // the parser can allocate, so hence needs the GC list
    source: []const u8,
    chunk: *Chunk,
) bool {
    var scanner = Scanner.init(source);
    var parser = Parser.init(alloc, vm_objects, &scanner, chunk);

    parser.advance();
    parser.expression();
    parser.consume(.EOF, "Expect end of expression");
    parser.endCompiler();

    return !parser.had_error;
}

const Precedence = enum {
    NONE,
    ASSIGNMENT,
    OR,
    AND,
    EQUALITY,
    COMPARISON,
    TERM,
    FACTOR,
    UNARY,
    CALL,
    PRIMARY,
};

const ParseRule = struct {
    prefix: ?*const (fn (*Parser) void),
    infix: ?*const (fn (*Parser) void),
    precedence: Precedence,
};

const parse_rules: [std.meta.fields(TokenType).len]ParseRule = init: {
    var rules: [std.meta.fields(TokenType).len]ParseRule = undefined;
    // zig fmt: off
    rules[@intFromEnum(TokenType.LEFT_PAREN)]  = .{ .prefix = &Parser.grouping, .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.RIGHT_PAREN)] = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.LEFT_CURLY)]  = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.RIGHT_CURLY)] = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.COMMA)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.DOT)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.MINUS)]       = .{ .prefix = &Parser.unary,    .infix = &Parser.binary, .precedence = .TERM };
    rules[@intFromEnum(TokenType.PLUS)]        = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .TERM };
    rules[@intFromEnum(TokenType.SEMI)]        = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.SLASH)]       = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .FACTOR };
    rules[@intFromEnum(TokenType.STAR)]        = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .FACTOR };
    rules[@intFromEnum(TokenType.NOT)]         = .{ .prefix = &Parser.unary,    .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.NOT_EQUAL)]   = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .EQUALITY };
    rules[@intFromEnum(TokenType.EQUAL)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.EQUAL_EQUAL)] = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .EQUALITY };
    rules[@intFromEnum(TokenType.GT)]          = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .COMPARISON };
    rules[@intFromEnum(TokenType.GEQ)]         = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .COMPARISON };
    rules[@intFromEnum(TokenType.LT)]          = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .COMPARISON };
    rules[@intFromEnum(TokenType.LEQ)]         = .{ .prefix = null,             .infix = &Parser.binary, .precedence = .COMPARISON };
    rules[@intFromEnum(TokenType.IDENTIFIER)]  = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.STRING)]      = .{ .prefix = &Parser.string,   .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.NUMBER)]      = .{ .prefix = &Parser.number,   .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.AND)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.CLASS)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.ELSE)]        = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FALSE)]       = .{ .prefix = &Parser.literal,  .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FOR)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FUN)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.IF)]          = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.NIL)]         = .{ .prefix = &Parser.literal,  .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.OR)]          = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.PRINT)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.RETURN)]      = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.SUPER)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.THIS)]        = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.TRUE)]        = .{ .prefix = &Parser.literal,  .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.VAR)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.WHILE)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.ERROR)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.EOF)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    // zig fmt: on
    break :init rules;
};

const Parser = struct {
    alloc: std.mem.Allocator,
    vm_objects: *?*_value.Obj,

    scanner: *Scanner,
    compiling_chunk: *Chunk,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    fn init(
        alloc: std.mem.Allocator,
        vm_objects: *?*_value.Obj,
        scanner: *Scanner,
        chunk: *Chunk,
    ) Parser {
        var parser = Parser{
            .alloc = alloc,
            .vm_objects = vm_objects,
            .scanner = scanner,
            .compiling_chunk = chunk,
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
        return parser;
    }

    fn advance(parser: *Parser) void {
        parser.previous = parser.current;

        while (true) {
            parser.current = parser.scanner.scanToken();
            if (parser.current.type != .ERROR) {
                break;
            }

            parser.errAtCurrent(parser.current.lexeme);
        }
    }

    fn expression(parser: *Parser) void {
        parser.parsePrecedence(.ASSIGNMENT);
    }

    fn consume(parser: *Parser, t: TokenType, message: []const u8) void {
        if (parser.current.type == t) {
            parser.advance();
            return;
        }

        parser.errAtCurrent(message);
    }

    fn currentChunk(parser: *Parser) *Chunk {
        // presumably this function will have different returns later
        return parser.compiling_chunk;
    }

    fn emitByte(parser: *Parser, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line);
    }

    fn emitOp(parser: *Parser, op: OpCode) void {
        parser.currentChunk().writeOp(op, parser.previous.line);
    }

    fn emitOps(parser: *Parser, op1: OpCode, op2: OpCode) void {
        parser.currentChunk().writeOp(op1, parser.previous.line);
        parser.currentChunk().writeOp(op2, parser.previous.line);
    }

    fn emitOpByte(parser: *Parser, op: OpCode, byte: u8) void {
        // I suspect this will be more useful than emitBytes
        parser.currentChunk().writeOp(op, parser.previous.line);
        parser.currentChunk().write(byte, parser.previous.line);
    }

    fn emitReturn(parser: *Parser) void {
        parser.emitOp(.RETURN);
    }

    fn makeConstant(parser: *Parser, value: Value) u8 {
        const constant = parser.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            parser.err("Too many constants in one chunk");
            return 0;
        }

        std.debug.assert(constant <= std.math.maxInt(u8));
        return @truncate(constant);
    }

    fn emitConstant(parser: *Parser, value: Value) void {
        parser.emitOpByte(.CONST, parser.makeConstant(value));
    }

    fn endCompiler(parser: *Parser) void {
        parser.emitReturn();
        if (debug_print_code) {
            if (!parser.had_error) {
                _debug.disassembleChunk(parser.currentChunk().*, "code");
            }
        }
    }

    fn number(parser: *Parser) void {
        const value = std.fmt.parseFloat(f64, parser.previous.lexeme) catch unreachable; // I don't we can error (?)
        parser.emitConstant(Value{ .NUMBER = value });
    }

    fn string(parser: *Parser) void {
        parser.emitConstant(Value{ .OBJ = @ptrCast(_object.copyString(
            parser.alloc,
            parser.vm_objects,
            parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1],
        )) });
    }

    fn grouping(parser: *Parser) void {
        parser.expression();
        parser.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    fn unary(parser: *Parser) void {
        const op_type = parser.previous.type;

        parser.parsePrecedence(.UNARY);

        switch (op_type) {
            .NOT => parser.emitOp(.NOT),
            .MINUS => parser.emitOp(.NEGATE),
            else => unreachable,
        }
    }

    fn binary(parser: *Parser) void {
        const op_type = parser.previous.type;
        const rule = getRule(op_type);
        parser.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (op_type) {
            .NOT_EQUAL => parser.emitOps(.EQUAL, .NOT),
            .EQUAL_EQUAL => parser.emitOp(.EQUAL),
            .GT => parser.emitOp(.GT),
            .GEQ => parser.emitOps(.LT, .NOT),
            .LT => parser.emitOp(.LT),
            .LEQ => parser.emitOps(.GT, .NOT),
            .PLUS => parser.emitOp(.ADD),
            .MINUS => parser.emitOp(.SUB),
            .STAR => parser.emitOp(.MUL),
            .SLASH => parser.emitOp(.DIV),
            else => unreachable,
        }
    }

    fn literal(parser: *Parser) void {
        switch (parser.previous.type) {
            .FALSE => parser.emitOp(.FALSE),
            .NIL => parser.emitOp(.NIL),
            .TRUE => parser.emitOp(.TRUE),
            else => unreachable,
        }
    }

    fn parsePrecedence(parser: *Parser, precedence: Precedence) void {
        parser.advance();
        const prefix_rule = getRule(parser.previous.type).prefix;
        if (prefix_rule == null) {
            parser.err("Expect expression");
            return;
        }
        std.debug.assert(prefix_rule != null);

        prefix_rule.?(parser);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type).precedence)) {
            parser.advance();
            const infix_rule = getRule(parser.previous.type).infix;
            std.debug.assert(infix_rule != null); // shoudl be guaranteed
            infix_rule.?(parser);
        }
    }

    fn getRule(t: TokenType) *const ParseRule {
        // we don't need this for the circularity reason that clox does
        // but it's nice to abstract away the @intFromEnum
        return &parse_rules[@intFromEnum(t)];
    }

    fn errAtCurrent(parser: *Parser, message: []const u8) void {
        parser.errAt(&parser.current, message);
    }

    fn err(parser: *Parser, message: []const u8) void {
        parser.errAt(&parser.previous, message);
    }

    fn errAt(parser: *Parser, token: *Token, message: []const u8) void {
        if (parser.panic_mode) return;
        parser.panic_mode = true;

        std.debug.print("[line {}] Error", .{token.line});
        if (token.type == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.type == .ERROR) {
            //
        } else {
            std.debug.print(" at {s}", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        parser.had_error = true;
    }
};
