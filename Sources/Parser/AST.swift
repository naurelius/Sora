



public struct File {
    public let elements: [Elememt]
}

public struct NodeID {
    let rawValue: UInt

    public init(_ rawValue: UInt) {
        self.rawValue = rawValue
    }

    public var value: UInt {
        rawValue
    }
}

public enum Elememt {
    case function(Function)
    case `class`(Class)
    case `struct`(Struct)
    case trait(Trait)
    case impl(Impl)
    case global(Global)
    case `enum`(Enum)
    case alias(Alias)
    case module(Module)
    case use(Use)
    case extern(ExternPackage)
    case error(id: NodeID, span: Span)
}

public struct Ident {
    public let name: Name
    public let span: Span
}

public struct Global {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let mutable: Bool
    public let dataType: Type
    public let initialValue: Expr?
    public let visibility: Visibility
}

public struct Module {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let elements: [Elememt]?
    public let visibility: Visibility
}

public struct Use {
    public let id: NodeID
    public let span: Span
    public let commonPath: [UsePathComponent]
    public let target: UseTargetDescriptor
}

public enum UseTargetDescriptor {
    case `default`
    case `as`(UseTargetName)
    case group(UseTargetGroup)
}

public struct UseTargetGroup {
    public let span: Span
    public let targets: [Use]
}

public struct UseTargetName {
    public let span: Span
    public let name: Ident?
}

public struct UsePathComponent {
    public let span: Span
    public let value: UsePathComponentValue
}

public enum UsePathComponentValue {
    case this
    case `super`
    case package
    case name(Ident)
    case error
}

public struct Const {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let dataType: Type
    public let expr: Expr
    public let visibility: Visibility
}

public struct Enum {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let typeParams: [TypeParam]?
    public let variants: [EnumVariant]
    public let visibility: Visibility
}

public struct EnumVariant {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let types: [Type]?
}

public struct Alias {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let type: Type
    public let visibility: Visibility
}

public struct Struct {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let fields: [StructField]
    public let visibility: Visibility
    public let `internal`: Bool
    public let typeParams: [TypeParam]?
}

public struct StructField {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let dataType: Type
    public let visibility: Visibility
}

public indirect enum Type {
    case this(TypeSelfType)
    case basic(TypeBasicType)
    case tuple(TypeTupleType)
    case lambda(TypeLambdaType)
    case error(id: NodeID, span: Span)
}

public struct TypeSelfType {
    public let id: NodeID
    public let span: Span
}

public struct TypeTupleType {
    public let id: NodeID
    public let span: Span
    public let subTypes: [Type]
}

public struct TypeLambdaType {
    public let id: NodeID
    public let span: Span
    public let params: [Type]
    public let ret: Type?
}

public struct TypeBasicType {
    public let id: NodeID
    public let span: Span
    public let path: Path
    public let params: [Type]
}

extension Type {
    public static func createSelf(id: NodeID, span: Span) -> Type {
        return .this(TypeSelfType(id: id, span: span))
    }

    public static func createBasic(id: NodeID, span: Span, path: Path, params: [Type]) -> Type {
        return .basic(TypeBasicType(id: id, span: span, path: path, params: params))
    }

    public static func createFct(id: NodeID, span: Span, params: [Type], ret: Type?) -> Type {
        return .lambda(TypeLambdaType(id: id, span: span, params: params, ret: ret))
    }

    public static func createTuple(id: NodeID, span: Span, subTypes: [Type]) -> Type {
        return .tuple(TypeTupleType(id: id, span: span, subTypes: subTypes))
    }

    public func toBasic() -> TypeBasicType? {
        switch self {
        case .basic(let val): return val
        default: return nil
        }
    }

    public func toTuple() -> TypeTupleType? {
        switch self {
        case .tuple(let val): return val
        default: return nil
        }
    }

    public func toFct() -> TypeLambdaType? {
        switch self {
        case .lambda(let val): return val
        default: return nil
        }
    }

    public func isUnit() -> Bool {
        switch self {
        case .tuple(let val) where val.subTypes.count == 0: return true
        default: return false
        }
    }

/*    public func toString(interner: Interner) -> String {
        switch self {
        case .this(_): return "Self"
        case .basic(let val): return "\(interner.str(name: val.n))"
        }
    }*/

    public func span() -> Span {
        switch self {
        case .this(let val): return val.span
        case .basic(let val): return val.span
        case .tuple(let val): return val.span
        case .lambda(let val): return val.span
        case .error(_, let s): return s
        }
    }

    public func id() -> NodeID {
        switch self {
        case .this(let val): return val.id
        case .basic(let val): return val.id
        case .tuple(let val): return val.id
        case .lambda(let val): return val.id
        case .error(let id, _): return id
        }
    }
}

public struct Impl {
    public let id: NodeID
    public let span: Span
    public let typeParams: [TypeParam]?
    public let traitType: Type?
    public let extendedType: Type
    public let methods: [Function]
}

public struct Trait {
    public let id: NodeID
    public let name: Ident?
    public let typeParams: [TypeParam]?
    public let span: Span
    public let methods: [Function]
    public let visibility: Visibility
}

public struct Class {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let `internal`: Bool
    public let visibility: Visibility
    public let fields: [Field]
    public let typeParams: [TypeParam]?
}

public struct ExternPackage {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let identifier: Ident?
}

public struct TypeParam {
    public let span: Span
    public let name: Ident?
    public let bounds: [Type]
}

public struct Field {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let dataType: Type
    public let primaryCtor: Bool
    public let expr: Expr?
    public let mutable: Bool
    public let visibility: Visibility
}

public enum FunctionKind {
    case function
    case lambda

    public func isLambda() -> Bool {
        switch self {
        case .lambda: return true
        case .function: return false
        }
    }
}

public struct Function {
    public let id: NodeID
    public let kind: FunctionKind
    public let span: Span
    public let name: Ident?
    public let isOptimizeImmediately: Bool
    public let visibility: Visibility
    public let isStatic: Bool
    public let isTest: Bool
    public let `internal`: Bool
    public let isConstructor: Bool
    public let params: [Param]
    public let returnType: Type?
    public let block: Expr?
    public let typeParams: [TypeParam]?

    public func getBlock() -> Expr {
        return self.block!
    }
}

public struct Modifiers {
    var inner: [ModifierElement]

    public init() {
        self.inner = []
    }

    public func contains(modifier: Annotation) -> Bool {
        return inner.contains(where: { $0.value == modifier })
    }

    public mutating func add(modifier: ModifierElement) {
        self.inner.append(modifier)
    }

    public func iter() -> IndexingIterator<[ModifierElement]> {
        return self.inner.makeIterator()
    }
}

public struct ModifierElement {
    public let value: Annotation
    public let span: Span
}

public struct AnnotationUsages {
    var inner: [AnnotationUsage]

    public init() {
        self.inner = []
    }

    public func contains(name: Name) -> Bool {
        self.inner.contains(where: {$0.name == name})
    }

    public mutating func add(usage: AnnotationUsage) {
        self.inner.append(usage)
    }

    public func iter() -> IndexingIterator<[AnnotationUsage]> {
        return self.inner.makeIterator()
    }
}

public struct AnnotationUsage {
    public let name: Name
    public let span: Span
    public let typeArgs: [Type]
    public let termArgs: [Expr]
}

public enum Annotation {
    case `internal`
    case pub
    case `static`
    case test
    case optimizeImmediately
    case error

    public func isError() -> Bool {
        return self == .error
    }

    public func name() -> StaticString {
        switch self {
        case .internal: return "internal"
        case .pub: return "pub"
        case .static: return "static"
        case .test: return "test"
        case .optimizeImmediately: return "optimizeImmediately"
        case .error: return "<error>"
        }
    }
}

public struct Param {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let mutable: Bool
    public let dataType: Type
    public let variadic: Bool
}

public enum Stmt {
    case `let`(StmtLetType)
    case expr(StmtExprType)

    public static func createLet(
        id: NodeID, 
        span: Span,
        pattern: LetPattern,
        dataType: Type?,
        expr: Expr?
    ) -> Stmt {
    return .let(StmtLetType(id: id, span: span, pattern: pattern, dataType: dataType, expr: expr
        ))
    }

    public static func createExpr(id: NodeID, span: Span, expr: Expr) -> Stmt {
        return .expr(StmtExprType(id: id, span: span, expr: expr))
    }

    public var span: Span {
        switch self {
        case .let(let stmt): return stmt.span
        case .expr(let stmt): return stmt.span
        }
    }

    public var isLet: Bool {
        if case .let(_) = self {
            return true
        } else {
            return false
        }
    }

    public var toLet: StmtLetType? {
        if case .let(let val) = self {
            return val
        } else {
            return nil
        }
    }

    public var isExpr: Bool {
        if case .expr(_) = self {
            return true
        } else {
            return false
        }
    }
   
    public var toExpr: StmtExprType? {
        if case .expr(let val) = self {
            return val
        } else {
            return nil
        }
    }
}

public struct StmtLetType {
    public let id: NodeID
    public let span: Span
    public let pattern: LetPattern
    public let dataType: Type?
    public let expr: Expr?
}

public enum LetPattern {
    case ident(LetIdentType)
    case tuple(LetTupleType)
    case underscore(LetUnderscoreType)

    public var isIdent: Bool {
        switch self {
        case .ident(_): return true
        default: return false
        }
    }

    public var isTuple: Bool {
        switch self {
        case .tuple(_): return true
        default: return false
        }
    }

    public var isUnderscore: Bool {
        switch self {
        case .underscore(_): return true
        default: return false
        }
    }

    public var toName: Name? {
        switch self {
        case .ident(let ident): return ident.name?.name
        default: return nil
        }
    }

    public var toIdent: LetIdentType? {
        if case .ident(let ident) = self {
            return ident
        } else {
            return nil
        }
    }

    public var toTuple: LetTupleType? {
        if case .tuple(let tuple) = self {
            return tuple
        } else {
            return nil
        }
    }
}

public struct LetUnderscoreType {
    public let id: NodeID
    public let span: Span
}

public struct LetIdentType {
    public let id: NodeID
    public let span: Span
    public let mutable: Bool
    public let name: Ident?
}

public struct LetTupleType {
    public let id: NodeID
    public let span: Span
    public let parts: [LetPattern]
}

public struct ExprForType {
    public let id: NodeID
    public let span: Span

    public let pattern: LetPattern
    public let expr: Expr
    public let block: Expr
}

public struct ExprWhileType {
    public let id: NodeID
    public let span: Span

    public let cond: Expr
    public let block: Expr
}

public struct StmtExprType {
    public let id: NodeID
    public let span: Span
    public let expr: Expr
}

public struct ExprReturnType {
    public let id: NodeID
    public let span: Span

    public let expr: Expr?
}

public struct ExprBreakType {
    public let id: NodeID
    public let span: Span
}

public struct ExprContinueType {
    public let id: NodeID
    public let span: Span
}

public enum UnOp {
    case plus, neg, not

    public func asString() -> StaticString {
        switch self {
        case .plus: return "+"
        case .neg: return "-"
        case .not: return "!"
        }
    }
}

public enum CmpOp {
    case eq, ne
    case lt, le, gt, ge
    case `is`, isNot

    public func asString() -> StaticString {
        switch self {
        case .eq: return "=="
        case .ne: return "!="
        case .lt: return "<"
        case .le: return "<="
        case .gt: return ">"
        case .ge: return ">="
        case .is: return "==="
        case .isNot: return "!=="
        }
    }
}

public enum BinOp {
    case assign
    case add, sub, div, mul, mod
    case cmp(CmpOp)
    case or, and, bitOr, bitAnd, bitXor
    case shiftL, arithShiftR, logicalShiftR

    public func asString() -> StaticString {
        switch self {
        case .assign: return "="
        case .add: return "+"
        case .sub: return "-"
        case .mul: return "*"
        case .div: return "/"
        case .mod: return "%"
        case .cmp(let op): return op.asString()
        case .or: return "||"
        case .and: return "&&"
        case .bitOr: return "|"
        case .bitAnd: return "&"
        case .bitXor: return "^"
        case .shiftL: return "<<"
        case .arithShiftR: return ">>"
        case .logicalShiftR: return ">>>"
        }
    }

    public var isAnyAssign: Bool {
        if case .assign = self {
            return true
        } else {
            return false
        }
    }

    public var isCompare: Bool {
        switch self {
        case .cmp(let cmp) where cmp != .is && cmp != .isNot: return true
        default: return false
        }
    }
}

public indirect enum Expr {
    case un(ExprUnType)
    case bin(ExprBinType)
    case litChar(ExprLitCharType)
    case litInt(ExprLitIntType)
    case litFloat(ExprLitFloatType)
    case litStr(ExprLitStrType)
    case template(ExprTemplateType)
    case litBool(ExprLitBoolType)
    case ident(ExprIdentType)
    case call(ExprCallType)
    case typeParam(ExprTypeParamType)
    case path(ExprPathType)
    case dot(ExprDotType)
    case this(ExprSelfType)
    case conv(ExprConvType)
    case lambda(Function)
    case block(ExprBlockType)
    case `if`(ExprIfType)
    case `for`(ExprForType)
    case `while`(ExprWhileType)
    case tuple(ExprTupleType)
    case paren(ExprParenType)
    case match(ExprMatchType)
    case `break`(ExprBreakType)
    case `continue`(ExprContinueType)
    case `return`(ExprReturnType)
    case error(id: NodeID, span: Span)
}

extension Expr {
    public static func createBlock(id: NodeID, span: Span, stmts: [Stmt], expr: Expr?) -> Expr {
        return .block(ExprBlockType(id: id, span: span, stmts: stmts, expr: expr))
    }

    public static func createIf(id: NodeID, span: Span, cond: Expr, thenBlock: Expr, elseBlock: Expr?) -> Expr {
        return .if(.init(id: id, span: span, cond: cond, thenBlock: thenBlock, elseBlock: elseBlock))
    }

    public static func createMatch(id: NodeID, span: Span, expr: Expr, cases: [MatchCaseType]) -> Expr {
        return .match(.init(id: id, span: span, expr: expr, cases: cases))
    }

    public static func createFor(id: NodeID, span: Span, pattern: LetPattern, expr: Expr, block: Expr) -> Expr {
        return .for(.init(id: id, span: span, pattern: pattern, expr: expr, block: expr))
    }

    public static func createWhile(id: NodeID, span: Span, cond: Expr, block: Expr) -> Expr {
        return .while(.init(id: id, span: span, cond: cond, block: block))
    }

    public static func createReturn(id: NodeID, span: Span, expr: Expr?) -> Expr {
        return .return(.init(id: id, span: span, expr: expr))
    }

    public static func createBreak(id: NodeID, span: Span) -> Expr {
        return .break(.init(id: id, span: span))
    }

    public static func createContinue(id: NodeID, span: Span) -> Expr {
        return .continue(.init(id: id, span: span))
    }

    public static func createUn(id: NodeID, span: Span, op: UnOp, opnd: Expr) -> Expr {
        return .un(.init(id: id, span: span, op: op, opnd: opnd))
    }

    public static func createBin(id: NodeID, span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
        return .bin(.init(id: id, span: span, op: op, initializer: false, lhs: lhs, rhs: rhs))
    }

    public static func createConv(id: NodeID, span: Span, object: Expr, dataType: Type) -> Expr {
        return .conv(.init(id: id, span: span, object: object, dataType: dataType))
    }

    public static func createLitChar(id: NodeID, span: Span, fullValue: String) -> Expr {
        return .litChar(.init(id: id, span: span, value: fullValue))
    }
}

public struct ExprIfType {
    public let id: NodeID
    public let span: Span

    public let cond: Expr
    public let thenBlock: Expr
    public let elseBlock: Expr?
}

public struct ExprTupleType {
    public let id: NodeID
    public let span: Span

    public let values: [Expr]
}

public struct ExprConvType {
    public let id: NodeID
    public let span: Span

    public let object: Expr
    public let dataType: Type
}

public struct ExprUnType {
    public let id: NodeID
    public let span: Span

    public let op: UnOp
    public let opnd: Expr
}

public struct ExprBinType {
    public let id: NodeID
    public let span: Span

    public let op: BinOp
    public let initializer: Bool
    public let lhs: Expr
    public let rhs: Expr
}

public struct ExprLitCharType {
    public let id: NodeID
    public let span: Span
    public let value: String
}

public struct ExprLitIntType {
    public let id: NodeID
    public let span: Span
    public let value: String
}

public struct ExprLitFloatType {
    public let id: NodeID
    public let span: Span
    public let value: String
}

public struct ExprLitStrType {
    public let id: NodeID
    public let span: Span
    public let value: String
}

public struct ExprTemplateType {
    public let id: NodeID
    public let span: Span
    public let parts: [Expr]
}

public struct ExprLitBoolType {
    public let id: NodeID
    public let span: Span
    public let value: Bool
}

public struct ExprBlockType {
    public let id: NodeID
    public let span: Span
    public let stmts: [Stmt]
    public let expr: Expr?
}

public struct ExprSelfType {
    public let id: NodeID
    public let span: Span
}

public struct ExprIdentType {
    public let id: NodeID
    public let span: Span
    public let name: Name
}

public struct ExprCallType {
    public let id: NodeID
    public let span: Span
    public let callee: Expr
    public let args: [Expr]

    public var object: Expr? {
        if let typeParam = self.callee.toTypeParam {
            if let dot = typeParam.callee.toDot {
                return dot.lhs
            } else {
                return nil
            }
        } else if let dot = self.callee.toDot {
            return dot.lhs
        } else {
            return nil
        }
    }

    public var objectOrCallee: Expr {
        return self.object ?? self.callee
    }
}

public struct ExprParenType {
    public let id: NodeID
    public let span: Span
    public let expr: Expr
}

public struct ExprMatchType {
    public let id: NodeID
    public let span: Span
    public let expr: Expr
    public let cases: [MatchCaseType]
}

public struct MatchCaseType {
    public let id: NodeID
    public let span: Span
    public let patterns: [MatchPattern]
    public let value: Expr
}

public struct MatchPattern {
    public let id: NodeID
    public let span: Span
    public let data: MatchPatternData
}

public enum MatchPatternData {
    case underscore
    case ident(MatchPatternIdent)
}

public struct MatchPatternIdent {
    public let path: Path
    public let params: [MatchPatternParam]?
}

public struct MatchPatternParam {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let mutable: Bool
}

public struct Path {
    public let id: NodeID
    public let span: Span
    public let names: [Ident]
}

public struct ExprTypeParamType {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let callee: Expr
    public let args: [Type]
}

public struct ExprPathType {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let lhs: Expr
    public let rhs: Expr
}

public struct ExprDotType {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let lhs: Expr
    public let rhs: Expr
}

public enum Visibility {
    case `public`
    case `default`

    public func fromModifiers(modifiers: Modifiers) -> Visibility {
        if modifiers.contains(modifier: .pub) {
            return .public
        } else {
            return .default
        }
    }

    public var isPublic: Bool {
        if case .public = self {
            return true
        } else {
            return false
        }
    }

    public var isDefault: Bool {
        if case .default = self {
            return true
        } else {
            return false
        }
    }
}


