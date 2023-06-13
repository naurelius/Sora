



// Super long and messy. Swift protocols works better
// TODO: Switch to prptocols

/// A full AST of Sora code 
/// Contains a list of declarations
public struct File {
    public let elements: [Decl]
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

//. A top level declaration in Sora code
public protocol Decl {}

/// An identifier in Sora code
public struct Ident {
    public let name: Name
    public let span: Span
}

/// A global variable
public struct Global: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let mutable: Bool
    public let dataType: Type
    public let initialValue: Expr?
    public let visibility: Visibility
}

/// A `module` declaration
public struct Module: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let elements: [Decl]?
    public let visibility: Visibility
}

/// A `use` declaration
public struct Use: Decl {
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

/// A `const` declaration
public struct Const: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let dataType: Type
    public let expr: Expr
    public let visibility: Visibility
}

/// An `enum` declaration
public struct Enum: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let typeParams: [TypeParam]?
    public let variants: [EnumVariant]
    public let visibility: Visibility
}

/// An enum variant
public struct EnumVariant {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let types: [Type]?
}

/// A type alias
public struct Alias: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let type: Type
    public let visibility: Visibility
}

/// A `struct` declaration
public struct Struct: Decl {
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

/// Represents a type in Sora code
public protocol Type {
    var id: NodeID { get }
    var span: Span { get }
}

///The self Type
public struct SelfType: Type {
    public let id: NodeID
    public let span: Span
}

///A tuple type
public struct TupleType: Type {
    public let id: NodeID
    public let span: Span
    public let subTypes: [Type]
}

/// A lambda or function type
public struct LambdaType: Type {
    public let id: NodeID
    public let span: Span
    public let params: [Type]
    public let ret: Type?
}

/// A basic type
public struct BasicType: Type {
    public let id: NodeID
    public let span: Span
    public let path: Path
    public let params: [Type]
}

public struct ErrorType: Type {
    public let id: NodeID
    public let span: Span
}

/// An `impl` declaration
public struct Impl: Decl {
    public let id: NodeID
    public let span: Span
    public let typeParams: [TypeParam]?
    public let traitType: Type?
    public let extendedType: Type
    public let methods: [Function]
}

/// A `trait` declaration
public struct Trait: Decl {
    public let id: NodeID
    public let name: Ident?
    public let typeParams: [TypeParam]?
    public let span: Span
    public let methods: [Function]
    public let visibility: Visibility
}

/// A `class` declaration
public struct Class: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let `internal`: Bool
    public let visibility: Visibility
    public let fields: [Field]
    public let typeParams: [TypeParam]?
}

/// An `extern` declaration
public struct ExternPackage: Decl {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let identifier: Ident?
}

/// An error declaration
public struct ErrorDecl: Decl {
    public let id: NodeID
    public let span: Span
}

/// A type parameter in Sora
public struct TypeParam {
    public let span: Span
    public let name: Ident?
    public let bounds: [Type]
}

/// A class field
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

/// Is the function a declaration or closure?
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

/// A function declaration
public struct Function: Decl {
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

/// An annotation
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

/// A function parameter
public struct Param {
    public let id: NodeID
    public let span: Span
    public let name: Ident?
    public let mutable: Bool
    public let dataType: Type
    public let variadic: Bool
}

/// Represents a Statement in Sora
public protocol Stmt {}

/// A `let` statement
public struct LetStmt: Stmt {
    public let id: NodeID
    public let span: Span
    public let pattern: LetPattern
    public let dataType: Type?
    public let expr: Expr?
}

/// Represents a let binding pattern
public protocol LetPattern {}

public extension LetPattern {
    var name: Name? {
        if let ident = self as? IdentPattern {
            return ident.name
        } else {
            return nil
        }
    }
}

/// An `_` binding pattern, the wildcard pattern
public struct UnderscorePattern: LetPattern {
    public let id: NodeID
    public let span: Span
}

/// An identifier binding pattern
public struct IdentPattern: LetPattern {
    public let id: NodeID
    public let span: Span
    public let mutable: Bool
    public let name: Ident?
}

/// A tuple binding pattern, a destructure
public struct TuplePattern: LetPattern {
    public let id: NodeID
    public let span: Span
    public let parts: [LetPattern]
}

/// A `for` loop
public struct ForExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let pattern: LetPattern
    public let expr: Expr
    public let block: Expr
}

/// A `while` loop
public struct WhileExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let cond: Expr
    public let block: Expr
}

/// An expression statement
public struct ExprStmt: Stmt {
    public let id: NodeID
    public let span: Span
    public let expr: Expr
}

/// A `return` expression
public struct ReturnExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let expr: Expr?
}

/// A break expression
public struct BreakExpr: Expr {
    public let id: NodeID
    public let span: Span
}

/// A continue expression
public struct ContinueExpr: Expr {
    public let id: NodeID
    public let span: Span
}

/// Unary operators
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

/// Comparison operators
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

/// Binary operators
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

/// Represents an expression in Sora code
public protocol Expr {
    var id: NodeID { get }
    var span: Span { get }
}

public extension Expr {
    var needsSemicolon: Bool {
        switch self {
            case is BlockExpr, is IfExpr, is MatchExpr, is ForExpr, is WhileExpr: return false
        default: return true
        }
    }

    var isLitTrue: Bool {
        if let litBool = self as? LitBoolExpr {
            return litBool.value == true
        } else {
            return false
        }
    }
}

/// A closure
public struct LambdaExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let value: Function
}

/// An error expression
public struct ErrorExpr: Expr {
    public let id: NodeID
    public let span: Span
}

/// An `if` expression
public struct IfExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let cond: Expr
    public let thenBlock: Expr
    public let elseBlock: Expr?
}

/// A tuple
public struct TupleExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let values: [Expr]
}

/// A type cast
public struct ConvExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let object: Expr
    public let dataType: Type
}

/// A unary expression
public struct UnExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let op: UnOp
    public let opnd: Expr
}

/// A binary expression
public struct BinExpr: Expr {
    public let id: NodeID
    public let span: Span

    public let op: BinOp
    public let initializer: Bool
    public let lhs: Expr
    public let rhs: Expr

    public init(id: NodeID, span: Span, op: BinOp, initializer: Bool = false, lhs: Expr, rhs: Expr) {
        self.id = id
        self.span = span
        self.op = op
        self.initializer = initializer
        self.lhs = lhs
        self.rhs = rhs
    }
}

/// A character literal
public struct LitCharExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let value: String
}

/// An integer literal
public struct LitIntExpr: Expr{
    public let id: NodeID
    public let span: Span
    public let value: String
}

/// A float literal
public struct LitFloatExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let value: String
}

/// A string literal
public struct LitStrExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let value: String
}

/// A string interpolation
public struct TemplateExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let parts: [Expr]
}

/// A boolean literal
public struct LitBoolExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let value: Bool
}

/// A block of code
public struct BlockExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let stmts: [Stmt]
    public let expr: Expr?
}

/// The self expression
public struct SelfExpr: Expr {
    public let id: NodeID
    public let span: Span
}

/// An identifier expression
public struct IdentExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let name: Name
}

/// A function call
public struct CallExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let callee: Expr
    public let args: [Expr]

    public var object: Expr? {
        if let typeParam = self.callee as? TypeParamExpr {
            if let dot = typeParam.callee as? DotExpr {
                return dot.lhs
            } else {
                return nil
            }
        } else if let dot = self.callee as? DotExpr {
            return dot.lhs
        } else {
            return nil
        }
    }

    public var objectOrCallee: Expr {
        return self.object ?? self.callee
    }
}

/// An expression in parenthesis
public struct ParenExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let expr: Expr
}

/// A `match` expression
public struct MatchExpr: Expr {
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

/// A match pattern
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

/// A path represents a fully qualified name
public struct Path {
    public let id: NodeID
    public let span: Span
    public let names: [Ident]
}

/// A type parameter expression
public struct TypeParamExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let callee: Expr
    public let args: [Type]
}

/// A fully qualified name as an expression
public struct PathExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let lhs: Expr
    public let rhs: Expr
}

/// A dot access expression
public struct DotExpr: Expr {
    public let id: NodeID
    public let span: Span
    public let opSpan: Span
    public let lhs: Expr
    public let rhs: Expr
}

/// Represents declaration visibility in Sora code
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


