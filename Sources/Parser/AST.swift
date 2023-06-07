



public struct File {
    public let elements: [Elememt]
}

public struct NodeID {
    let rawValue: UInt
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
