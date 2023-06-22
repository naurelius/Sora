



























public final class Parser {
    var tokens: [TokenKind]
    var tokenWidths: [UInt32]
    var tokenIdx: Int
    var idGen: NodeIdGenerator
    public private(set) var interner: Interner
    let content: String
    var errors: [ParseErrorWithLocation]
    var nodes: [(UInt, UInt32)]
    var offset: UInt32
    
    public init(with content: String, interner: inout Interner) {
        let result = lex(content: content)
        self.tokens = result.tokens
        self.tokenWidths = result.widths
        self.tokenIdx = 0
        self.idGen = NodeIdGenerator()
        self.offset = 0
        self.interner = interner
        self.content = content
        self.errors = result.errors
        self.nodes = []
    }

    func generateId() -> NodeID {
        return idGen.next()
    }

    public func parse() -> (ast: File, gen: NodeIdGenerator, errors: [ParseErrorWithLocation]) {
        let file = parseFile()
        assert(nodes.isEmpty)
        return (ast: file, gen: idGen, errors: errors)
    }

    func parseFile() -> File {
        skipTrivia()
        var decls = [Decl]()
        while !isEof() {
            let decl = parseDecl()
            decls.append(decl)
        }
        return File(elements: decls)
    }

    func parseDecl() -> Decl {
        let modifiers = parseModifiers()

        switch current {
        case .fn: do {
            restrictModifiers(modifiers: modifiers, restrict: [.internal, .optimizeImmediately, .test, .pub])
            return parseFunction(modifiers: modifiers)
        }
        case .class: do {
            restrictModifiers(modifiers: modifiers, restrict: [.internal, .pub])
            return parseClass(modifiers: modifiers)
        }
        case .struct: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub, .internal])
            return parseStruct(modifiers: modifiers)
        }
        case .trait: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseTrait(modifiers: modifiers)
        }
        case .impl: do {
            banModifiers(modifiers: modifiers)
            return parseImpl()
        }
        case .alias: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseAlias(modifiers: modifiers)
        }
        case .let: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseGlobal(modifiers: modifiers)
        }
        case .const: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseConst(modifiers: modifiers)
        }
        case .enum: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseEnum(modifiers: modifiers)
        }
        case .mod: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseModule(modifiers: modifiers)
        }
        case .use: do {
            restrictModifiers(modifiers: modifiers, restrict: [.pub])
            return parseUse()
        }
        case .extern: do {
            banModifiers(modifiers: modifiers)
            return parseExtern()
        }
        default: do {
            let span = currentSpan
            reportErrorAt(msg: .expectedTopLevelDeclaration, span: span)
            advance()
            return ErrorDecl(id: generateId(), span: span)
        }

        }
    }

    func parseExtern() -> ExternPackage {
        startNode()
        assertToken(kind: .extern)
        expect(kind: .package)
        let name = expectIdentifier()
        let identifier = eat(kind: .as) ? expectIdentifier() : nil

        return ExternPackage(id: generateId(), span: finishNode(), name: name, identifier: identifier)
    }

    func parseUse() -> Use {
        assertToken(kind: .use)
        let useDecl = parseUseInner()
        expect(kind: .semicolon)
        return useDecl
    }

    func parseUseInner() -> Use {
        startNode()
        var path = [UsePathComponent]()
        var allowBrace = false

        while true {
            if self.is(kind: .lbrace) {
                allowBrace = true
                break
            }
            let component = parseUsePathComponent()
            path.append(component)

            if !eat(kind: .colon_colon) {
                break
            }
        }

        let target: UseTargetDescriptor
        if allowBrace && self.is(kind: .lbrace) {
            target = parseUseBrace()
        } else if self.is(kind: .as) {
            target = .as(parseUseAs())
        } else {
            target = .default
        }

        return Use(id: generateId(), span: finishNode(), commonPath: path, target: target)
    }

    func parseUseAs() -> UseTargetName {
        startNode()
        assertToken(kind: .as)
        let name = eat(kind: .underscore) ? nil : expectIdentifier()

        return UseTargetName(span: finishNode(), name: name)
    }

    func parseUsePathComponent() -> UsePathComponent {
        startNode()
        let value: UsePathComponentValue
        if eat(kind: .this) {
            value = .this
        } else if eat(kind: .package) {
            value = .package
        } else if eat(kind: .super) {
            value = .super
        } else {
            let name = expectIdentifier()
            if let name = name {
                value = .name(name)
            } else {
                value = .error
            }
        }
        return UsePathComponent(span: finishNode(), value: value)
    }

    func parseUseBrace() -> UseTargetDescriptor {
        startNode()
        assertToken(kind: .lbrace)
        let targets = parseList(sep: .comma, stop: .rbrace) { $0.parseUseInner() }

        return .group(.init(span: finishNode(), targets: targets))
    }

    func parseEnum(modifiers: Modifiers) -> Enum {
        startNode()
        assertToken(kind: .enum)
        let name = expectIdentifier()
        let typeParams = parseTypeParams()

        expect(kind: .lbrace)
        let variants = parseList(sep: .comma, stop: .rbrace) { $0.parseEnumVariant() }
        return Enum(
            id: generateId(),
            span: finishNode(),
            name: name,
            typeParams: typeParams,
            variants: variants,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseModule(modifiers: Modifiers) -> Module {
        startNode()
        assertToken(kind: .mod)
        let name = expectIdentifier()
        var decls: [Decl]?
        if eat(kind: .lbrace) {
            decls = []
            while !self.is(kind: .rbrace) && !isEof() {
                decls?.append(parseDecl())
            }
            expect(kind: .rbrace)
        } else {
            expect(kind: .semicolon)
            decls = nil
        }

        return Module(
            id: generateId(),
            span: finishNode(),
            name: name,
            elements: decls,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseEnumVariant() -> EnumVariant {
        startNode()
        let name = expectIdentifier()
        let types = eat(kind: .lparen) ? parseList(sep: .comma, stop: .rparen) { $0.parseType() } : nil
        return EnumVariant(id: generateId(), span: finishNode(), name: name, types: types)
    }

    func parseConst(modifiers: Modifiers) -> Const {
        assertToken(kind: .const)
        let name = expectIdentifier()
        expect(kind: .colon)
        let type = parseType()
        let expr = parseExpression()
        expect(kind: .semicolon)

        return Const(
            id: generateId(),
            span: finishNode(),
            name: name,
            dataType: type,
            expr: expr,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseImpl() -> Impl {
        startNode()
        assertToken(kind: .impl)

        let typeParams = parseTypeParams()

        let typeName = parseType()

        let (classType, traitType) = eat(kind: .for) ? (parseType(), typeName) : (typeName, nil)

        expect(kind: .lbrace)

        var methods = [Function]()

        while !self.is(kind: .rbrace) {
            let modifiers = parseModifiers()

            restrictModifiers(modifiers: modifiers, restrict: [.static, .internal, .pub])

            if self.is(kind: .fn) {
                let method = parseFunction(modifiers: modifiers)
                methods.append(method)
            } else {
                reportError(msg: .expectedImplElement)
                advance()
            }
        }

        expect(kind: .rbrace)
        return Impl(
            id: generateId(),
            span: finishNode(),
            typeParams: typeParams,
            traitType: traitType,
            extendedType: classType,
            methods: methods
            )

    }

    func parseGlobal(modifiers: Modifiers) -> Global {
        startNode()
        assertToken(kind: .let)

        let mutable = eat(kind: .mut)
        let name = expectIdentifier()

        expect(kind: .colon)
        let dataType = parseType()
        let expr = eat(kind: .eq) ? parseExpression() : nil

        expect(kind: .semicolon)

        return Global(
            id: generateId(),
            span: finishNode(),
            name: name,
            mutable: mutable,
            dataType: dataType,
            initialValue: expr,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseTrait(modifiers: Modifiers) -> Trait {
        startNode()
        assertToken(kind: .trait)
        let name = expectIdentifier()
        let typeParams = parseTypeParams()

        expect(kind: .lbrace)
        var methods = [Function]()

        while !self.is(kind: .rbrace) && !isEof() {
            let modifiers = parseModifiers()
            restrictModifiers(modifiers: modifiers, restrict: [.static])
            let function = parseFunction(modifiers: modifiers)
            methods.append(function)
        }

        expect(kind: .rbrace)
        return Trait(
            id: generateId(),
            name: name,
            typeParams: typeParams,
            span: finishNode(),
            methods: methods,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseStruct(modifiers: Modifiers) -> Struct {
        startNode()
        assertToken(kind: .struct)
        let ident = expectIdentifier()
        let typeParams = parseTypeParams()

        let fields: [StructField]

        if eat(kind: .lparen) {
            fields = parseList(sep: .comma, stop: .rparen) { $0.parseStructField() }
        } else if eat(kind: .lbrace) {
            fields = parseList(sep: .comma, stop: .rbrace) { $0.parseStructField() }
        } else {
            fields = []
        }

        return Struct(
            id: generateId(),
            span: finishNode(),
            name: ident,
            fields: fields,
            visibility: .fromModifiers(modifiers: modifiers),
            internal: modifiers.contains(modifier: .internal),
            typeParams: typeParams
            )
    }

    func parseStructField() -> StructField {
        startNode()

        let modifiers = parseModifiers()
        restrictModifiers(modifiers: modifiers, restrict: [.pub])

        let ident = expectIdentifier()

        expect(kind: .colon)
        let ty = parseType()

        return StructField(
            id: generateId(),
            span: finishNode(),
            name: ident,
            dataType: ty,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseClass(modifiers: Modifiers) -> Class {
        startNode()
        assertToken(kind: .class)
        let name = expectIdentifier()
        let typeParams = parseTypeParams()

        let fields: [Field]

        if eat(kind: .lparen) {
            fields = parseList(sep: .comma, stop: .rparen) { $0.parseClassField() }
        } else if eat(kind: .lbrace) {
            fields = parseList(sep: .comma, stop: .rbrace) { $0.parseClassField() }
        } else {
            fields = []
        }

        return Class(
            id: generateId(),
            span: finishNode(),
            name: name,
            internal: modifiers.contains(modifier: .internal),
            visibility: .fromModifiers(modifiers: modifiers),
            fields: fields,
            typeParams: typeParams
            )
    }

    func parseClassField() -> Field {
        startNode()
        let modifiers = parseModifiers()
        restrictModifiers(modifiers: modifiers, restrict: [.pub])

        let name = expectIdentifier()

        expect(kind: .colon)
        let dataType = parseType()
       
        return .init(
            id: generateId(),
            span: finishNode(),
            name: name,
            dataType: dataType,
            primaryCtor: false,
            expr: nil,
            mutable: true,
            visibility: .fromModifiers(modifiers: modifiers)
            )
    }

    func parseAlias(modifiers: Modifiers) -> Alias {
        startNode()
        assertToken(kind: .alias)
        let name = expectIdentifier()
        expect(kind: .eq)
        let ty = parseType()
        expect(kind: .semicolon)

        return Alias(id: generateId(), span: finishNode(), name: name, type: ty, visibility: .fromModifiers(modifiers: modifiers))
    }

    func parseTypeParams() -> [TypeParam]? {
        if eat(kind: .lbracket) {
            let params = parseList(sep: .comma, stop: .rbracket) { $0.parseTypeParam() }
            return params
        } else {
            return nil
        }
    }

    func parseTypeParam() -> TypeParam {
        startNode()
        let name = expectIdentifier()

        var bounds: [Type] = []

        if eat(kind: .colon) {
            
            while true {
                bounds.append(parseType())
                if !eat(kind: .add) {
                    break
                }
            }

        }
        return .init(span: finishNode(), name: name, bounds: bounds)
    }

    func parseModifiers() -> Modifiers {
        var modifiers = Modifiers()
        while true {
            let modifier = parseModifier()
            if modifier == nil {
                break
            }
            let mod = modifier!
            if !mod.value.isError() && modifiers.contains(modifier: mod.value) {
                reportError(msg: .redundantAnnotation(String(describing: mod.value.name())))
                continue
            }
            modifiers.add(modifier: mod)
        }
        return modifiers
    }

    func parseModifier() -> ModifierElement? {
        startNode()
        if eat(kind: .pub) {
            return .init(value: .pub, span: finishNode())
        } else if eat(kind: .static) {
            return .init(value: .static, span: finishNode())
        } else if eat(kind: .at) {
            if eat(kind: .pub) {
                return .init(value: .pub, span: finishNode())
            } else if eat(kind: .static) {
                return .init(value: .static, span: finishNode())
            }

            let ident = expectIdentifier()
            let annotation: Annotation
            if let ident = ident {
                switch interner.str(name: ident.name) {
                case "internal": annotation = .internal
                case "pub": annotation = .pub
                case "static": annotation = .static
                case "Test": annotation = .test
                case "optimizeImmediately": annotation = .optimizeImmediately
                case let anno: do {
                    reportErrorAt(msg: .unknownAnnotation(anno), span: ident.span)
                    annotation = .error
                }
                }
            } else {
                annotation = .error
            }
            return .init(value: annotation, span: finishNode())
        } else {
            abandonNode()
            return nil
        }
    }

    func banModifiers(modifiers: Modifiers) {
        restrictModifiers(modifiers: modifiers, restrict: [])
    }

    func restrictModifiers(modifiers: Modifiers, restrict: [Annotation]) {
        for modifier in modifiers.iter() {
            if modifier.value.isError() {
                continue
            }
            if !restrict.contains(modifier.value) {
                reportErrorAt(msg: .misplacedAnnptation(String(describing: modifier.value.name())), span: modifier.span)
            }
        }
    }

    func parseFunction(modifiers: Modifiers) -> Function {
        startNode()
        assertToken(kind: .fn)
        let name = expectIdentifier()
        let typeParams = parseTypeParams()
        let params = parseFunctionParams()
        let returnType = parseFunctionType()
        let block = parseFunctionBlock()

        return Function(
            id: generateId(),
            kind: .function,
            span: finishNode(),
            name: name,
            isOptimizeImmediately: modifiers.contains(modifier: .optimizeImmediately),
            visibility: .fromModifiers(modifiers: modifiers),
            isStatic: modifiers.contains(modifier: .static),
            isTest: modifiers.contains(modifier: .test),
            internal: modifiers.contains(modifier: .internal),
            isConstructor: false,
            params: params,
            returnType: returnType,
            block: block,
            typeParams: typeParams
            )
    }

    func parseFunctionParams() -> [Param] {
        if expect(kind: .lparen) {
            return parseList(sep: .comma, stop: .rparen) { $0.parseFunctionParam() }
        } else {
            return []
        }
    }

    func parseList<R>(sep: TokenKind, stop: TokenKind, _ parse: (Parser) -> R) -> [R] {
        var data: [R] = []
        var comma = true

        while !`is`(kind: stop) && !isEof() {
            if !comma {
                guard let sepName = tokenName(kind: sep) else { fatalError("missing name") }
                reportError(msg: .expectedToken(.init(describing: sepName)))
                break
            }
            let entry = parse(self)
            data.append(entry)
            comma = eat(kind: sep)
        }
        expect(kind: stop)
        return data
    }

    func parseFunctionParam() -> Param {
        startNode()
        let mutable = eat(kind: .mut)
        let name = expectIdentifier()

        expect(kind: .colon)
        let dataType = parseType()
        let variadic = eat(kind: .dot_dot_dot)

        return Param(id: generateId(), span: finishNode(), name: name, mutable: mutable, dataType: dataType, variadic: variadic)
    }

    func parseFunctionType() -> Type? {
        if eat(kind: .colon) {
            return parseType()
        } else {
            return nil
        }
    }

    func parseFunctionBlock() -> Expr? {
        if eat(kind: .semicolon) {
            return nil
        } else {
            return parseBlock()
        }
    }

    func parseType() -> Type {
        switch current {
        case .capitalThis: do {
            let span = currentSpan
            assertToken(kind: .capitalThis)
            return SelfType(id: generateId(), span: span)
        }
        case .identifier: do {
            startNode()
            let path = parsePath()
            let params = eat(kind: .lbracket) ? parseList(sep: .comma, stop: .rbracket) { $0.parseType() } : []
            return BasicType(id: generateId(), span: finishNode(), path: path, params: params)
        }
        case .lparen: do {
            startNode()
            assertToken(kind: .lparen)
            let subTypes = parseList(sep: .comma, stop: .rparen) { $0.parseType() }
            if eat(kind: .colon) {
                let ret = parseType()
                return LambdaType(id: generateId(), span: finishNode(), params: subTypes, ret: ret)
            } else {
                return TupleType(id: generateId(), span: finishNode(), subTypes: subTypes)
            }
        }
        default: do {
            let span = currentSpan
            reportError(msg: .expectedType)
            return ErrorType(id: generateId(), span: span)
        }
        }
    }

    func parsePath() -> Path {
        startNode()
        var names = [Ident]()
        assert(current == .identifier)
        let name = expectIdentifier()
        if let name = name {
            names.append(name)
        }

        while eat(kind: .colon_colon) {
            let name = expectIdentifier()
            if let name = name {
                names.append(name)
            } else {
                break
            }
        }

        return Path(id: generateId(), span: finishNode(), names: names)
    }

    func parseLet() -> Stmt {
        startNode()

        assertToken(kind: .let)
        let pattern = parseLetPattern()
        let dataType = parseVarType()
        let expr = parseVarAssign()

        expect(kind: .semicolon)
        return LetStmt(id: generateId(), span: finishNode(), pattern: pattern, dataType: dataType, expr: expr)
    }

    func parseLetPattern() -> LetPattern {
        startNode()
        if eat(kind: .lparen) {
            let parts = parseList(sep: .comma, stop: .rparen) { $0.parseLetPattern() }
            return TuplePattern(id: generateId(), span: finishNode(), parts: parts)
        } else if eat(kind: .underscore) {
            return UnderscorePattern(id: generateId(), span: finishNode())
        } else {
            let mutable = eat(kind: .mut)
            let name = expectIdentifier()
            return IdentPattern(id: generateId(), span: finishNode(), mutable: mutable, name: name)
        }
    }

    func parseVarType() -> Type? {
        if eat(kind: .colon) {
            return parseType()
        } else {
            return nil
        }
    }

    func parseVarAssign() -> Expr? {
        if eat(kind: .eq) {
            return parseExpression()
        } else {
            return nil
        }
    }

    func parseBlock() -> Expr {
        startNode()
        var stmts = [Stmt]()
        var expr: Expr? = nil

        if expect(kind: .lbrace) {
            while !`is`(kind: .rbrace) && !isEof() {
                let stmtOrExpr = parseStmtOrExpr()
                switch stmtOrExpr {
                case .stmt(let stmt): stmts.append(stmt)
                case .expr(let currExpr): do {
                    if currExpr.needsSemicolon {
                        expr = currExpr
                        break
                    } else if !`is`(kind: .rbrace) {
                        stmts.append(ExprStmt(id: generateId(), span: currExpr.span, expr: currExpr))
                    } else {
                        expr = currExpr
                    }
                }
                }
            }
            expect(kind: .rbrace)
        }
        return BlockExpr(id: generateId(), span: finishNode(), stmts: stmts, expr: expr)
    }

    func parseStmtOrExpr() -> StmtOrExpr {
        switch current {
        case .let: return .stmt(parseLet())
        default: do {
            let expr = parseExpression()
            if eat(kind: .semicolon) {
                let span = spanFrom(start: expr.span.start)
                return .stmt(ExprStmt(id: generateId(), span: span, expr: expr))
            } else {
                return .expr(expr)
            }
        }
        }
    }

    func parseIf() -> Expr {
        startNode()
        assertToken(kind: .if)

        let cond = parseExpression()
        let thenBlock = parseBlock()
        
        let elseBlock: Expr?
        if eat(kind: .else) {
            if self.is(kind: .if) {
                elseBlock = parseIf()
            } else {
                elseBlock = parseBlock()
            }
        } else {
            elseBlock = nil
        }

        return IfExpr(id: generateId(), span: finishNode(), cond: cond, thenBlock: thenBlock, elseBlock: elseBlock)
    }

    func parseMatch() -> Expr {
        startNode()
        assertToken(kind: .match)
        let expr = parseExpression()
        var cases = [MatchCaseType]()
        var comma = true

        expect(kind: .lbrace)

        while !`is`(kind: .rbrace) && !isEof() {
            if !comma {
                reportError(msg: .expectedToken(","))
                break
            }
            let case_ = parseMatchCase()
            cases.append(case_)
            comma = eat(kind: .comma)
        }
        expect(kind: .rbrace)

        return MatchExpr(id: generateId(), span: finishNode(), expr: expr, cases: cases)
    }

    func parseMatchCase() -> MatchCaseType {
        startNode()
        var patterns: [MatchPattern] = []
        patterns.append(parseMatchPattern())

        while eat(kind: .or) {
            patterns.append(parseMatchPattern())
        }
        expect(kind: .double_arrow)
        let value = parseExpression()
        return .init(id: generateId(), span: finishNode(), patterns: patterns, value: value)
    }

    func parseMatchPattern() -> MatchPattern {
        startNode()
        let data: MatchPatternData
        if eat(kind: .underscore) {
            data = .underscore
        } else {
            let path = parsePath()
            let params = eat(kind: .lparen) ? parseList(sep: .comma, stop: .rparen) { $0.parseMatchPatternParam() } : nil

            data = .ident(.init(path: path, params: params))
        }
        return .init(id: generateId(), span: finishNode(), data: data)
    }

    func parseMatchPatternParam() -> MatchPatternParam {
        startNode()
        let (mutable, name): (Bool, Ident?)
        if eat(kind: .underscore) {
            (mutable, name) = (false, nil)
        } else {
            let mut = eat(kind: .mut)
            let ident = expectIdentifier()
            (mutable, name) = (mut, ident)
        }

        return .init(id: generateId(), span: finishNode(), name: name, mutable: mutable)
    }

    func parseFor() -> Expr {
        startNode()
        assertToken(kind: .for)
        let pattern = parseLetPattern()
        expect(kind: .in)
        let expr = parseExpression()
        let block = parseBlock()

        return ForExpr(id: generateId(), span: finishNode(), pattern: pattern, expr: expr, block: block)
    }

    func parseWhile() -> Expr {
        startNode()
        assertToken(kind: .while)
        let expr = parseExpression()
        let block = parseBlock()

        return WhileExpr(id: generateId(), span: finishNode(), cond: expr, block: block)
    }

    func parseBreak() -> Expr {
        startNode()
        assertToken(kind: .break)
        return BreakExpr(id: generateId(), span: finishNode())
    }

    func parseContinue() -> Expr {
        startNode()
        assertToken(kind: .continue)
        return ContinueExpr(id: generateId(), span: finishNode())
    }

    func parseReturn() -> Expr {
        startNode()
        assertToken(kind: .return)
        let expr = `is`(kind: .semicolon) ? nil : parseExpression()
        return ReturnExpr(id: generateId(), span: finishNode(), expr: expr)
    }

    func parseExpression() -> Expr {
        return parseBinary(prec: 0)
    }

    func parseBinary(prec: Int) -> Expr {
        if !isSet(set: EXPRESSION_FIRST) {
            reportError(msg: .expectedExpression)
            return ErrorExpr(id: generateId(), span: currentSpan)
        }
        let start = currentSpan.start
        var left = parseUnary()
        while true {
            let rightPrec: Int
            switch current {
            case .eq: rightPrec = 1
            case .or_or: rightPrec = 2
            case .and_and: rightPrec = 3
            case .eq_eq, .not_eq, .lt, .le, .gt, .ge, .eq_eq_eq, .not_eq_eq: rightPrec = 4
            case .add, .sub, .caret, .or: rightPrec = 5
            case .mul, .div, .modulo, .and, .lt_lt, .gt_gt, .gt_gt_gt: rightPrec = 6
            case .as: rightPrec = 7
            default: return left
            }

            if prec >= rightPrec {
                return left
            }
            let kind = current
            advance()
            switch kind {
            case .as: do {
                let right = parseType()
                let span = spanFrom(start: start)
                left = ConvExpr(id: generateId(), span: span, object: left, dataType: right)
            }
            default: do {
                let right = parseBinary(prec: rightPrec)
                left =  createBinary(kind: kind, start: start, left: left, right: right)
            }
            }
        }
    }

    func parseUnary() -> Expr {
        switch current {
        case .add, .sub, .not: do {
            startNode()
            let kind = current
            advance()
            let op: UnOp
            switch kind {
            case .add: op = .plus
            case .sub: op = .neg
            case .not: op = .not
            default: fatalError("unreachable!")
            }
            let expr = parsePrimary()
            return UnExpr(id: generateId(), span: finishNode(), op: op, opnd: expr)
        }
        default: return parsePrimary()
        }
    }

    func parsePrimary() -> Expr {
        let start = currentSpan.start
        var left = parseFactor()

        while true {
            switch current {
            case .dot: do {
                let opSpan = currentSpan
                assertToken(kind: .dot)
                let rhs = parseFactor()
                let span = spanFrom(start: start)
                left =  DotExpr(id: generateId(),
                               span: span,
                               opSpan: opSpan, 
                               lhs: left,
                               rhs: rhs)
            }
            case .lparen: do {
                assertToken(kind: .lparen)
                let args = parseList(sep: .comma, stop: .rparen, { $0.parseExpression() })
                let span = spanFrom(start: start)
                left = CallExpr(id: generateId(), span: span, callee: left, args: args)
            }
            case .lbracket: do {
                let opSpan = currentSpan
                assertToken(kind: .lbracket)
                let types = parseList(sep: .comma, stop: .rbracket, { $0.parseType() })
                let span = spanFrom(start: start)
                left = TypeParamExpr(id: generateId(), span: span, opSpan: opSpan, callee: left, args: types)
            }
            case .colon_colon: do {
                let opSpan = currentSpan
                assertToken(kind: .colon_colon)
                let rhs = parseFactor()
                let span = spanFrom(start: start)
                left = PathExpr(id: generateId(), span: span, opSpan: opSpan, lhs: left, rhs: rhs)
            }
            default: return left
            }
        }
    }
    
    func createBinary(kind: TokenKind, start: UInt32, left: Expr, right: Expr) -> Expr {
        //print("entered func createBinary, kind: \(kind), left: \(left),  right: \(right)")
        let op: BinOp
        switch kind {
        case .eq: op = .assign
        case .or_or: op = .or
        case .and_and: op = .and
        case .eq_eq: op = .cmp(.eq)
        case .not_eq: op = .cmp(.ne)
        case .lt: op = .cmp(.lt)
        case .le: op = .cmp(.le)
        case .gt: op = .cmp(.gt)
        case .ge: op = .cmp(.ge)
        case .eq_eq_eq: op = .cmp(.is)
        case .not_eq_eq: op = .cmp(.isNot)
        case .or: op = .bitOr
        case .and: op = .bitAnd
        case .caret: op = .bitXor
        case .add: op = .add
        case .sub: op = .sub
        case .mul: op = .mul
        case .div: op = .div
        case .modulo: op = .mod
        case .lt_lt: op = .shiftL
        case .gt_gt: op = .arithShiftR
        case .gt_gt_gt: op = .logicalShiftR
        default: fatalError("unimplemented token \(kind)")
        }
        let span = spanFrom(start: start)
        return BinExpr(id: generateId(), span: span, op: op, lhs: left, rhs: right)
    }

    func parseFactor() -> Expr {
        let span = currentSpan
        switch current {
        case .lparen: return parseParentheses()
        case .lbrace: return parseBlock()
        case .if: return parseIf()
        case .charLiteral: return parseLitChar()
        case .intLiteral: return parseLitInt()
        case .floatLiteral: return parseLitFloat()
        case .stringLiteral: return parseString()
        case .templateLiteral: return parseTemplate()
        case .identifier: return parseIdentifier()
        case .true: return parseBoolLiteral()
        case .false: return parseBoolLiteral()
        case .this: return parseThis()
        case .or, .or_or: return parseLambda()
        case .for: return parseFor()
        case .while: return parseWhile()
        case .break: return parseBreak()
        case .continue: return parseContinue()
        case .return: return parseReturn()
        case .match: return parseMatch()
        default: do {
            reportError(msg: .expectedFacor)
            return ErrorExpr(id: generateId(), span: span)
        }
        }
    }
    
    func parseIdentifier() -> Expr {
        guard let ident = expectIdentifier() else {
            fatalError("identifier expected")
        }

        return IdentExpr(id: generateId(), span: ident.span, name: ident.name)
    }

    func parseParentheses() -> Expr {
        startNode()
        assertToken(kind: .lparen)

        if eat(kind: .rparen) {
            return TupleExpr(
                id: generateId(),
                span: finishNode(),
                values: []
                )
        }

        let expr = parseExpression()

        if current == .comma {
            var values = [expr]
            while true {
                expect(kind: .comma)
                if eat(kind: .rparen) {
                    break
                }
                let expr = parseExpression()
                values.append(expr)
                if eat(kind: .rparen) {
                    break
                }
            }

            return TupleExpr(id: generateId(), span: finishNode(), values: values)
        } else {
            expect(kind: .rparen)
            return ParenExpr(id: generateId(), span: finishNode(), expr: expr)
        }
    }

    func parseLitChar() -> Expr {
        let span = currentSpan
        assertToken(kind: .charLiteral)
        let value = sourceSpan(span: span)
        return LitCharExpr(id: generateId(), span: span, value: value)
    }

    func parseLitInt() -> Expr {
        let span = currentSpan
        assertToken(kind: .intLiteral)
        let value = sourceSpan(span: span)
        return LitIntExpr(id: generateId(), span: span, value: value)
    }

    func parseLitFloat() -> Expr {
        let span = currentSpan
        assertToken(kind: .floatLiteral)
        let value = sourceSpan(span: span)
        return LitFloatExpr(id: generateId(), span: span, value: value)
    }

    func parseTemplate() -> Expr {
        let span = currentSpan
        let start = span.start
        assertToken(kind: .templateLiteral)
        let value = sourceSpan(span: span)

        var parts: [Expr] = []
        parts.append(LitStrExpr(id: generateId(), span: span, value: value))

        var done = false
        
        while !done {
            let expr = parseExpression()
            parts.append(expr)
            let span = currentSpan
            if !`is`(kind: .templateLiteral) {
                done = true
            }
            if !`is`(kind: .templateLiteral) && !`is`(kind: .templateEndLiteral) {
                reportError(msg: .unclosedStringTemplate)
                break
            }
            let value = sourceSpan(span: span)
            parts.append(LitStrExpr(id: generateId(), span: span, value: value))
            advance()
        }
        let span2 = spanFrom(start: start)
        return TemplateExpr(id: generateId(), span: span2, parts: parts)
    }

    func parseString() -> Expr {
        let span = currentSpan
        assertToken(kind: .stringLiteral)
        let value = sourceSpan(span: span)
        return LitStrExpr(id: generateId(), span: span, value: value)
    }

    func parseBoolLiteral() -> Expr {
        let span = currentSpan
        let kind = current
        assertToken(kind: kind)
        let value = kind == .true
        return LitBoolExpr(id: generateId(), span: span, value: value)
    }

    func parseThis() -> Expr {
        let span = currentSpan
        assertToken(kind: .this)
        return SelfExpr(id: generateId(), span: span)
    }

    func parseLambda() -> Expr {
        startNode()
        let params: [Param]
        if eat(kind: .or_or) {
            params = []
        } else {
            assertToken(kind: .or)
            params = parseList(sep: .comma, stop: .or, { $0.parseFunctionParam() })
        }
        
        let returnType = eat(kind: .colon) ? parseType() : nil
        let block = parseBlock()
        let function = Function(
            id: generateId(),
            kind: .lambda,
            span: finishNode(),
            name: nil,
            isOptimizeImmediately: false,
            visibility: .default,
            isStatic: false,
            isTest: false,
            internal: false,
            isConstructor: false,
            params: params,
            returnType: returnType,
            block: block,
            typeParams: nil
            )
        return LambdaExpr(id: function.id, span: function.span, value: function)
    }

    func assertToken(kind: TokenKind, line: Int = #line) {
        assert(eat(kind: kind), "\(line)")
    }


    func expectIdentifier() -> Ident? {
        let span = currentSpan
        if eat(kind: .identifier) {
            let value = sourceSpan(span: span)
            let name = interner.intern(name: value)
            return Ident(name: name, span: span)
        } else {
            reportErrorAt(msg: .expectedIdentifier, span: span)
            return nil
        }
    }

    @discardableResult
    func expect(kind: TokenKind) -> Bool {
        #if DEBUG
        assert(tokenName(kind: kind) != nil)
        #endif
        if eat(kind: kind) {
            return true
        } else {
            guard let kind = tokenName(kind: kind) else { preconditionFailure("missing name") }
            reportError(msg: .expectedToken(.init(describing: kind)))
            return false
        }
    }

    @discardableResult
    func eat(kind: TokenKind) -> Bool {
        if current == kind {
            advance()
            return true
        } else {
            return false
        }
    }

    func reportError(msg: ParseError) {
        reportErrorAt(msg: msg, span: currentSpan)
    }

    func reportErrorAt(msg: ParseError, span: Span) {
        errors.append(.init(span: span, error: msg))
    }

    func advance() {
        rawAdvance()
        skipTrivia()
    }

    func skipTrivia() {
        while current.isTrivia() {
            rawAdvance()
        }
    }

    func rawAdvance() {
        if tokenIdx < tokens.count {
            let kind = current
            let len = tokenWidths[tokenIdx]
            offset += len
            assert(kind <= .eof)
            tokenIdx += 1
        }
    }

    var current: TokenKind {
        if tokenIdx < tokens.count {
            return tokens[tokenIdx]
        } else {
            return .eof
        }
    }

    var currentSpan: Span {
        if tokenIdx < tokens.count {
            let length = tokenWidths[tokenIdx]
            return Span(start: offset, len: length)
        } else {
            return .at(start: offset)
        }
    }

    func `is`(kind: TokenKind) -> Bool {
        current == kind
    }

    func isSet(set: TokenSet) -> Bool {
        return set.contains(kind: current)
    }

    func isEof() -> Bool {
        current == .eof
    }

    func startNode() {
        nodes.append((UInt(tokenIdx), offset))
    }

    func finishNode() -> Span {
        guard let last = self.nodes.popLast() else {
            fatalError("missing node start")
        }
        let (startToken, startOffset) = last
        
        var endToken = tokenIdx - 1
        assert(endToken < tokens.count)
        var endOffset = offset

        while endToken > startToken {
            if !tokens[Int(endToken)].isTrivia() {
                break
            }
            endOffset -= tokenWidths[Int(endToken)]
            endToken -= 1
        }
        return Span(start: startOffset, len: endOffset - startOffset)
    }

    func abandonNode() {
        guard nodes.last != nil else {
            fatalError("missing node start")
        }
        _ = nodes.popLast()
    }

    func sourceSpan(span: Span) -> String {
        let start = content.index(content.startIndex, offsetBy: Int(span.start))
        let end = content.index(content.startIndex, offsetBy: Int(span.end))
        return String(content[start..<end])
    }

    func spanFrom(start: UInt32) -> Span {
        return .init(start: start, len: offset - start)
    }
}

enum StmtOrExpr {
    case stmt(Stmt)
    case expr(Expr)
}

func tokenName(kind: TokenKind) -> StaticString? {
    switch kind {
    case .package: return "package"
    case .in: return "in"
    case .eq: return "="
    case .comma: return ","
    case .semicolon: return ";"
    case .dot: return "."
    case .colon: return ":"
    case .arrow: return "->"
    case .double_arrow: return "=>"
    case .or: return "|"
    case .lparen: return "("
    case .rparen: return ")"
    case .lbracket: return "["
    case .rbracket: return "]"
    case .lbrace: return "{"
    case .rbrace: return "}"
    default: return nil
    }
}

public struct NodeIdGenerator {
    var value: UInt

    public init() {
        value = 1
    }

    public mutating func next() -> NodeID {
        let value = self.value
        self.value += 1
        return NodeID(value)
    }
}
