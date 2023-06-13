

















public struct Builder {
    public func buildBlock() -> BuilderBlock {
        return BuilderBlock()
    }

    public func buildFct(name: Name) -> BuilderFct {
        return BuilderFct(name: name)
    }

    public func buildInitializerAssign(
        id: NodeID,
        span: Span,
        lhs: Expr,
        rhs: Expr
        ) -> Expr {
        return BinExpr(
            id: id,
            span: span,
            op: .assign,
            initializer: true,
            lhs: lhs,
            rhs: rhs
            )
    }

    public func buildIdent(id: NodeID, span: Span, name: Name) -> Expr {
        return IdentExpr(id: id, span: span, name: name)
    }
}

public struct BuilderFct {
    let name: Name
    let visibility: Visibility
    let isCtor: Bool
    let returnType: Type?
    var params: [Param]
    var block: Expr?

    public init(name: Name) {
        self.name = name
        visibility = Visibility.public
        isCtor = false
        returnType = nil
        params = []
        block = nil
    }

    public mutating func setBlock(block: Expr) -> Self {
        self.block = block
        return self
    }

    public func build(id: NodeID, span: Span) -> Function {
        return Function(
            id: id,
            kind: .function,
            span: span,
            name: Ident(name: self.name, span: span),
            isOptimizeImmediately: false,
            visibility: self.visibility,
            isStatic: false,
            isTest: false,
            internal: false,
            isConstructor: false,
            params: self.params,
            returnType: self.returnType,
            block: self.block,
            typeParams: nil
            )
    }
}

public struct BuilderBlock {
    var stmts: [Stmt]
    public init() {
        stmts = []
    }

    public mutating func addExpr(id: NodeID, expr: Expr) -> Self {
        let stmt = ExprStmt(id: id, span: expr.span, expr: expr)
        stmts.append(stmt)
        return self
    }
    public func build(id: NodeID, span: Span) -> Expr {
        return BlockExpr(id: id, span: span, stmts: stmts, expr: nil)
    }
}
