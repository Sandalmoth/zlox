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
const ObjFunction = _object.ObjFunction;

const _debug = @import("debug.zig");

const debug_print_code = true;

pub fn compile(
    alloc: std.mem.Allocator,
    vm_objects: *?*_value.Obj, // the parser can allocate, so hence needs the GC list
    source: []const u8,
) ?*ObjFunction {
    var scanner = Scanner.init(source);
    var compiler = Compiler.init(.SCRIPT);
    var parser = Parser.init(alloc, vm_objects, &scanner, &compiler);
    compiler.init2(alloc, vm_objects);

    parser.advance();

    while (!parser.match(.EOF)) {
        parser.declaration();
    }

    const function = parser.endCompiler();
    return if (parser.had_error) null else function;
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
    prefix: ?*const (fn (*Parser, bool) void),
    infix: ?*const (fn (*Parser, bool) void),
    precedence: Precedence,
};

const Local = struct {
    name: Token,
    depth: i32,
};

const FunctionType = enum {
    FUNCTION,
    SCRIPT,
};

// why the actual fuck do we have a separate compiler and parser?
// i assume it has something to do with local vars in functions?
// and clox makes it a global too :(
// hopefully just storing a reference in the parser should work
const Compiler = struct {
    enclosing: ?*Compiler,
    function: *ObjFunction,
    type: FunctionType,

    locals: [std.math.maxInt(u8) + 1]Local,
    n_locals: usize,
    scope_depth: i32,

    fn init(typ: FunctionType) Compiler {
        return Compiler{
            .enclosing = null, // i'll do the init of this explicitly
            .function = undefined, // should be fine?
            .type = typ,
            .locals = undefined, // we shouldn't be accessing unused ones anyway
            .n_locals = 0,
            .scope_depth = 0,
        };
    }

    // by god what have I done
    fn init2(compiler: *Compiler, alloc: std.mem.Allocator, vm_objects: *?*_value.Obj) void {
        // slight out of order compared to clox...
        compiler.function = _object.newFunction(alloc, vm_objects);

        var local = &compiler.locals[compiler.n_locals];
        compiler.n_locals += 1;
        local.depth = 0;
        local.name.lexeme = ""; // are empty strings allowed?
    }
};

const parse_rules: [std.meta.fields(TokenType).len]ParseRule = init: {
    var rules: [std.meta.fields(TokenType).len]ParseRule = undefined;
    // zig fmt: off
    rules[@intFromEnum(TokenType.LEFT_PAREN)]  = .{ .prefix = &Parser.grouping, .infix = &Parser.call,   .precedence = .CALL };
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
    rules[@intFromEnum(TokenType.IDENTIFIER)]  = .{ .prefix = &Parser.variable, .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.STRING)]      = .{ .prefix = &Parser.string,   .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.NUMBER)]      = .{ .prefix = &Parser.number,   .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.AND)]         = .{ .prefix = null,             .infix = &Parser.and_,   .precedence = .AND };
    rules[@intFromEnum(TokenType.CLASS)]       = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.ELSE)]        = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FALSE)]       = .{ .prefix = &Parser.literal,  .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FOR)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.FUN)]         = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.IF)]          = .{ .prefix = null,             .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.NIL)]         = .{ .prefix = &Parser.literal,  .infix = null,           .precedence = .NONE };
    rules[@intFromEnum(TokenType.OR)]          = .{ .prefix = null,             .infix = &Parser.or_,    .precedence = .OR };
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
    compiler: *Compiler,
    compiling_chunk: *Chunk,

    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    fn init(
        alloc: std.mem.Allocator,
        vm_objects: *?*_value.Obj,
        scanner: *Scanner,
        compiler: *Compiler,
    ) Parser {
        var parser = Parser{
            .alloc = alloc,
            .vm_objects = vm_objects,
            .scanner = scanner,
            .compiler = compiler,
            .compiling_chunk = undefined,
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

    fn block(parser: *Parser) void {
        while (!parser.check(.RIGHT_CURLY) and !parser.check(.EOF)) {
            parser.declaration();
        }

        parser.consume(.RIGHT_CURLY, "Expect '}' after block");
    }

    fn function(parser: *Parser, typ: FunctionType) void {
        var compiler = Compiler.init(typ);
        compiler.init2(parser.alloc, parser.vm_objects);
        compiler.enclosing = parser.compiler;
        parser.compiler = &compiler;
        parser.compiler.function.name = _object.copyString(parser.alloc, parser.vm_objects, parser.previous.lexeme);
        parser.beginScope();

        parser.consume(.LEFT_PAREN, "expect '(' after function name");
        if (!parser.check(.RIGHT_PAREN)) {
            while (true) {
                parser.compiler.function.arity += 1;
                if (parser.compiler.function.arity > 255) {
                    parser.errAtCurrent("A function can't have more than 255 arguments");
                }
                const constant = parser.parseVariable("Expect argument name");
                parser.defineVariable(constant);

                if (!parser.match(.COMMA)) {
                    break;
                }
            } // it's a sad day when you need do/while and the language doesn't support it
        }
        parser.consume(.RIGHT_PAREN, "expect ')' after arguments");
        parser.consume(.LEFT_CURLY, "expect '{' before function body");
        parser.block();

        var _function = parser.endCompiler();
        parser.compiler = compiler.enclosing.?;
        parser.emitOpByte(
            .CONST,
            parser.makeConstant(Value{ .OBJ = @ptrCast(_function) }),
        );
    }

    fn funDeclaration(parser: *Parser) void {
        const global = parser.parseVariable("Expect function name");
        parser.markInitialized();
        parser.function(.FUNCTION);
        parser.defineVariable(global);
    }

    fn varDeclaration(parser: *Parser) void {
        const global = parser.parseVariable("Expect variable name");

        if (parser.match(.EQUAL)) {
            parser.expression();
        } else {
            parser.emitOp(.NIL); // aah, so an empty declaration sets to nil
        }
        parser.consume(.SEMI, "Expect ';' after variable declaration");

        parser.defineVariable(global);
    }

    fn expressionStatement(parser: *Parser) void {
        parser.expression();
        parser.consume(.SEMI, "Expect ';' after expression");
        parser.emitOp(.POP);
    }

    fn forStatement(parser: *Parser) void {
        parser.beginScope();
        parser.consume(.LEFT_PAREN, "Expect '(' after 'for'");
        if (parser.match(.SEMI)) {
            // no initializer here yo
        } else if (parser.match(.VAR)) {
            parser.varDeclaration();
        } else {
            parser.expressionStatement();
        }

        var loop_start = parser.currentChunk().code.items.len;
        var exit_jump: ?usize = null;
        if (!parser.match(.SEMI)) {
            parser.expression();
            parser.consume(.SEMI, "Expect ';' after loop condition");

            exit_jump = parser.emitJump(.JUMP_IF_FALSE);
            parser.emitOp(.POP);
        }

        if (!parser.match(.RIGHT_PAREN)) {
            const body_jump = parser.emitJump(.JUMP);
            const increment_start = parser.currentChunk().code.items.len;
            parser.expression();
            parser.emitOp(.POP);
            parser.consume(.RIGHT_PAREN, "Expect ')' after for clauses");

            parser.emitLoop(loop_start);
            loop_start = increment_start;
            parser.patchJump(body_jump);
        }

        parser.statement();
        parser.emitLoop(loop_start);

        if (exit_jump != null) {
            parser.patchJump(exit_jump.?);
            parser.emitOp(.POP);
        }

        parser.endScope();
    }

    fn ifStatement(parser: *Parser) void {
        parser.consume(.LEFT_PAREN, "Expect '(' after 'if'");
        parser.expression();
        parser.consume(.RIGHT_PAREN, "Expect ')' after condition");

        const then_jump = parser.emitJump(.JUMP_IF_FALSE);
        parser.emitOp(.POP); // get rid of the condition
        parser.statement();
        const else_jump = parser.emitJump(.JUMP);
        parser.patchJump(then_jump);
        parser.emitOp(.POP); // get rid of the condition
        if (parser.match(.ELSE)) {
            parser.statement();
        }
        parser.patchJump(else_jump);
    }

    fn printStatement(parser: *Parser) void {
        parser.expression();
        parser.consume(.SEMI, "Expect ';' after value");
        parser.emitOp(.PRINT);
    }

    fn returnStatement(parser: *Parser) void {
        if (parser.compiler.type == .SCRIPT) {
            parser.err("Can't return from top level code");
        }

        if (parser.match(.SEMI)) {
            parser.emitReturn();
        } else {
            parser.expression();
            parser.consume(.SEMI, "Expect ';' after return value");
            parser.emitOp(.RETURN);
        }
    }

    fn whileStatement(parser: *Parser) void {
        const loop_start = parser.currentChunk().code.items.len;
        parser.consume(.LEFT_PAREN, "Expect '(' after 'while'");
        parser.expression();
        parser.consume(.RIGHT_PAREN, "Expect ')' after condition");

        const exit_jump = parser.emitJump(.JUMP_IF_FALSE);
        parser.emitOp(.POP);
        parser.statement();
        parser.emitLoop(loop_start);

        parser.patchJump(exit_jump);
        parser.emitOp(.POP);
    }

    fn synchronize(parser: *Parser) void {
        parser.panic_mode = false;
        while (parser.current.type != .EOF) {
            if (parser.previous.type == .SEMI) return;
            switch (parser.current.type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }

            parser.advance();
        }
    }

    fn declaration(parser: *Parser) void {
        if (parser.match(.FUN)) {
            parser.funDeclaration();
        } else if (parser.match(.VAR)) {
            parser.varDeclaration();
        } else {
            parser.statement();
        }

        if (parser.panic_mode) {
            parser.synchronize();
        }
    }

    fn statement(parser: *Parser) void {
        if (parser.match(.PRINT)) {
            parser.printStatement();
        } else if (parser.match(.FOR)) {
            parser.forStatement();
        } else if (parser.match(.IF)) {
            parser.ifStatement();
        } else if (parser.match(.RETURN)) {
            parser.returnStatement();
        } else if (parser.match(.WHILE)) {
            parser.whileStatement();
        } else if (parser.match(.LEFT_CURLY)) {
            parser.beginScope();
            parser.block();
            parser.endScope();
        } else {
            parser.expressionStatement();
        }
    }

    fn consume(parser: *Parser, t: TokenType, message: []const u8) void {
        if (parser.current.type == t) {
            parser.advance();
            return;
        }

        parser.errAtCurrent(message);
    }

    fn match(parser: *Parser, t: TokenType) bool {
        if (!parser.check(t)) {
            return false;
        }
        parser.advance();
        return true;
    }

    inline fn check(parser: *Parser, t: TokenType) bool {
        return parser.current.type == t;
    }

    fn currentChunk(parser: *Parser) *Chunk {
        // HAH, indeed it did serve a point
        return &parser.compiler.function.chunk;
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

    fn emitLoop(parser: *Parser, loop_start: usize) void {
        parser.emitOp(.LOOP);

        const offset = parser.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            parser.err("Loop body too large");
        }

        parser.emitByte(@intCast((offset >> 8) & 0xff));
        parser.emitByte(@intCast(offset & 0xff));
    }

    fn emitJump(parser: *Parser, op: OpCode) usize {
        parser.emitOp(op);
        parser.emitByte(0xff);
        parser.emitByte(0xff);
        return parser.currentChunk().code.items.len - 2;
    }

    fn emitReturn(parser: *Parser) void {
        parser.emitOp(.NIL);
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

    fn patchJump(parser: *Parser, offset: usize) void {
        const jump = parser.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            parser.err("Too much code to jump over");
        }

        parser.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        parser.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn endCompiler(parser: *Parser) *ObjFunction {
        parser.emitReturn();

        const _function = parser.compiler.function;

        if (debug_print_code) {
            if (!parser.had_error) {
                _debug.disassembleChunk(
                    parser.currentChunk().*,
                    if (_function.name != null)
                        _function.name.?.chars[0.._function.name.?.len]
                    else
                        "<script>",
                );
            }
        }

        return _function;
    }

    fn beginScope(parser: *Parser) void {
        parser.compiler.scope_depth += 1;
    }

    fn endScope(parser: *Parser) void {
        parser.compiler.scope_depth -= 1;

        while (parser.compiler.n_locals > 0 and
            parser.compiler.locals[parser.compiler.n_locals - 1].depth > parser.compiler.scope_depth)
        {
            parser.emitOp(.POP);
            parser.compiler.n_locals -= 1;
        }
    }

    fn number(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        const value = std.fmt.parseFloat(f64, parser.previous.lexeme) catch unreachable; // I don't we can error (?)
        parser.emitConstant(Value{ .NUMBER = value });
    }

    fn or_(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        const else_jump = parser.emitJump(.JUMP_IF_FALSE);
        const end_jump = parser.emitJump(.JUMP);

        parser.patchJump(else_jump);
        parser.emitOp(.POP);

        parser.parsePrecedence(.OR);
        parser.patchJump(end_jump);
    }

    fn string(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        parser.emitConstant(Value{ .OBJ = @ptrCast(_object.copyString(
            parser.alloc,
            parser.vm_objects,
            parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1],
        )) });
    }

    fn namedVariable(parser: *Parser, name: Token, can_assign: bool) void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg = parser.resolveLocal(name);
        if (arg != -1) {
            getOp = .GET_LOCAL;
            setOp = .SET_LOCAL;
        } else {
            arg = parser.identifierConstant(name);
            getOp = .GET_GLOBAL;
            setOp = .SET_GLOBAL;
        }
        std.debug.assert(arg >= 0);
        std.debug.assert(arg < std.math.maxInt(u8));

        if (can_assign and parser.match(.EQUAL)) {
            parser.expression();
            parser.emitOpByte(setOp, @intCast(arg));
        } else {
            parser.emitOpByte(getOp, @intCast(arg));
        }
    }

    fn variable(parser: *Parser, can_assign: bool) void {
        parser.namedVariable(parser.previous, can_assign);
    }

    fn grouping(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        parser.expression();
        parser.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    fn unary(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        const op_type = parser.previous.type;

        parser.parsePrecedence(.UNARY);

        switch (op_type) {
            .NOT => parser.emitOp(.NOT),
            .MINUS => parser.emitOp(.NEGATE),
            else => unreachable,
        }
    }

    fn binary(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
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

    fn call(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        const n_args = parser.argumentList();
        parser.emitOpByte(.CALL, n_args);
    }

    fn literal(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
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

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
        prefix_rule.?(parser, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type).precedence)) {
            parser.advance();
            const infix_rule = getRule(parser.previous.type).infix;
            std.debug.assert(infix_rule != null); // shoudl be guaranteed
            infix_rule.?(parser, can_assign);
        }

        if (can_assign and parser.match(.EQUAL)) {
            parser.err("Invalid assignment target");
        }
    }

    fn identifierConstant(parser: *Parser, name: Token) u8 {
        return parser.makeConstant(Value{ .OBJ = @ptrCast(
            _object.copyString(parser.alloc, parser.vm_objects, name.lexeme),
        ) });
    }

    fn identifiersEqual(a: Token, b: Token) bool {
        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn addLocal(parser: *Parser, name: Token) void {
        if (parser.compiler.n_locals == std.math.maxInt(u8)) {
            parser.err("Too many local variables in function");
            return;
        }

        var local = &parser.compiler.locals[parser.compiler.n_locals];
        parser.compiler.n_locals += 1;
        local.name = name;
        local.depth = -1;
    }

    fn declareVariable(parser: *Parser) void {
        if (parser.compiler.scope_depth == 0) {
            return;
        }

        const name = parser.previous;

        // minor rewrite to prevent underflow of usize
        var i = parser.compiler.n_locals;
        while (i > 0) : (i -= 1) {
            const local = parser.compiler.locals[i - 1];
            if (local.depth != -1 and local.depth < parser.compiler.scope_depth) {
                break;
            }

            if (identifiersEqual(name, local.name)) {
                parser.err("Already a variable with this name in this scope");
            }
        }

        parser.addLocal(name);
    }

    fn parseVariable(parser: *Parser, error_message: []const u8) u8 {
        parser.consume(.IDENTIFIER, error_message);

        parser.declareVariable();
        if (parser.compiler.scope_depth > 0) return 0;

        return parser.identifierConstant(parser.previous);
    }

    fn markInitialized(parser: *Parser) void {
        if (parser.compiler.scope_depth == 0) {
            return;
        }

        parser.compiler.locals[parser.compiler.n_locals - 1].depth =
            parser.compiler.scope_depth;
    }

    fn defineVariable(parser: *Parser, global: u8) void {
        if (parser.compiler.scope_depth > 0) {
            parser.markInitialized();
            return;
        }

        parser.emitOpByte(.DEFINE_GLOBAL, global);
    }

    fn argumentList(parser: *Parser) u8 {
        var n_args: u8 = 0;
        if (!parser.check(.RIGHT_PAREN)) {
            while (true) {
                parser.expression();
                if (n_args == 255) {
                    parser.err("Cannot have more than 255 arguments");
                }
                n_args += 1;
                if (!parser.match(.COMMA)) {
                    break;
                }
            }
        }
        parser.consume(.RIGHT_PAREN, "Expect ')' after arguments");
        return n_args;
    }

    fn and_(parser: *Parser, can_assign: bool) void {
        _ = can_assign;
        const end_jump = parser.emitJump(.JUMP_IF_FALSE);

        parser.emitOp(.POP);
        parser.parsePrecedence(.AND);

        parser.patchJump(end_jump);
    }

    fn resolveLocal(parser: *Parser, name: Token) i32 {
        var i = parser.compiler.n_locals;
        while (i > 0) : (i -= 1) {
            const local = parser.compiler.locals[i - 1];
            if (Parser.identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    parser.err("Can't read local variable in its own initializer");
                }
                return @intCast(i - 1);
            }
        }

        return -1;
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
