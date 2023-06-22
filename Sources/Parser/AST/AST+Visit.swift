



public protocol Visitor { 
    mutating func visit(ast: File)
    mutating func visit(extern: ExternPackage)
    mutating func visit(global: Global)
    mutating func visit(trait: Trait)
    mutating func visit(impl: Impl)
    mutating func visit(clazz: Class)
    mutating func visit(struc: Struct)
    mutating func visit(const: Const)
    mutating func visit(en: Enum)
    mutating func visit(alias: Alias)
    mutating func visit(mod: Module)
    mutating func visit(use: Use)
    mutating func visit(stfield: StructField)
    mutating func visit(ctor: Function)
    mutating func visit(method: Function)
    mutating func visit(field: Field)
    mutating func visit(fct: Function)
    mutating func visit(param: Param)
    mutating func visit(type: Type)
    mutating func visit(stmt: Stmt)
    mutating func visit(expr: Expr)
}

public extension Visitor {
    mutating func visit(ast: File) {
        walk(v: &self, ast: ast)
    }
    mutating func visit(extern: ExternPackage) {
        walk(v: &self, extern: extern)
    }
    mutating func visit(global: Global) {
        walk(v: &self, global: global)
    }
    mutating func visit(trait: Trait) {
        walk(v: &self, trait: trait)
    }
    mutating func visit(impl: Impl) {
        walk(v: &self, impl: impl)
    }
    mutating func visit(clazz: Class) {
        walk(v: &self, cls: clazz)
    }
    mutating func visit(struc: Struct) {
        walk(v: &self, struc: struc)
    }
    mutating func visit(const: Const) {
        walk(v: &self, const: const)
    }
    mutating func visit(en: Enum) {
        walk(v: &self, en: en)
    }
    mutating func visit(alias: Alias) {
        walk(v: &self, alias: alias)
    }
    mutating func visit(mod: Module) {
        walk(v: &self, module: mod)
    }
    mutating func visit(use: Use) {
        walk(v: &self, use: use)
    }
    mutating func visit(stfield: StructField) {
        walk(v: &self, stfield: stfield)
    }
    mutating func visit(ctor: Function) {
        walk(v: &self, fct: ctor)
    }
    mutating func visit(method: Function) {
        walk(v: &self, fct: method)
    }
    mutating func visit(field: Field) {
        walk(v: &self, field: field)
    }
    mutating func visit(fct: Function) {
        walk(v: &self, fct: fct)
    }
    mutating func visit(param: Param) {
        walk(v: &self, param: param)
    }
    mutating func visit(type: Type) {
        walk(v: &self, type: type)
    }
    mutating func visit(stmt: Stmt) {
        walk(v: &self, stmt: stmt)
    }
    mutating func visit(expr: Expr) {
        walk(v: &self, expr: expr)
    }


}

public func walk<V: Visitor>(v: inout V, ast: File) {
    for decl in ast.elements {
        walk(v: &v, decl: decl)
    }
}

public func walk<V: Visitor>(v: inout V, decl: Decl) {
    switch decl {
    case is Function: v.visit(fct: decl as! Function)
    case is Class: v.visit(clazz: decl as! Class)
    case is Struct: v.visit(struc: decl as! Struct)
    case is Trait: v.visit(trait: decl as! Trait)
    case is Impl: v.visit(impl: decl as! Impl)
    case is Global: v.visit(global: decl as! Global)
    case is Const: v.visit(const: decl as! Const)
    case is Alias: v.visit(alias: decl as! Alias)
    case is Enum: v.visit(en: decl as! Enum)
    case is Module: v.visit(mod: decl as! Module)
    case is Use: v.visit(use: decl as! Use)
    case is ExternPackage: v.visit(extern: decl as! ExternPackage)
    case is ErrorDecl: break
    default: fatalError("unreachable!")
    }
}

public func walk<V: Visitor>(v: inout V, global: Global) {
    v.visit(type: global.dataType)
    if let initialValue = global.initialValue {
        v.visit(expr: initialValue)
    }
}

public func walk<V: Visitor>(v: inout V, trait: Trait) {
    for m in trait.methods {
        v.visit(method: m)
    }
}

public func walk<V: Visitor>(v: inout V, impl: Impl) {
    for m in impl.methods {
        v.visit(method: m)
    }
}

public func walk<V: Visitor>(v: inout V, cls: Class) {
    for f in cls.fields {
        v.visit(field: f)
    }
}

public func walk<V: Visitor>(v: inout V, const: Const) {
    v.visit(type: const.dataType)
    v.visit(expr: const.expr)
}

public func walk<V: Visitor>(v: inout V, en: Enum) {
    // nothing to do...
}

public func walk<V: Visitor>(v: inout V, alias: Alias) {
    v.visit(type: alias.type)
}

public func walk<V: Visitor>(v: inout V, module: Module) {
    if let elements = module.elements {
        for decl in elements {
            walk(v: &v, decl: decl)
        }
    }
}

public func walk<V: Visitor>(v: inout V, use: Use) {
    // nothing to do...
}

public func walk<V: Visitor>(v: inout V, extern: ExternPackage) {
    // nothing to do...
}

public func walk<V: Visitor>(v: inout V, struc: Struct) {
    for f in struc.fields {
        v.visit(stfield: f)
    }
}

public func walk<V: Visitor>(v: inout V, stfield: StructField) {
    v.visit(type: stfield.dataType)
}

public func walk<V: Visitor>(v: inout V, field: Field) {
    v.visit(type: field.dataType)
}

public func walk<V: Visitor>(v: inout V, fct: Function) {
    for p in fct.params {
        v.visit(param: p)
    }

    if let ty = fct.returnType {
        v.visit(type: ty)
    }

    if let block = fct.block {
        v.visit(expr: block)
    }
}

public func walk<V: Visitor>(v: inout V, param: Param) {
    v.visit(type: param.dataType)
}

public func walk<V: Visitor>(v: inout V, type: Type) {
    switch type {
    case is BasicType, is SelfType: break
    case is TupleType:
        let tuple = type as! TupleType
        for ty in tuple.subTypes {
            v.visit(type: ty)
        }
    case is LambdaType: 
        let fct = type as! LambdaType
        fct.params.forEach { v.visit(type: $0) }
    case is ErrorType: do {}
    default: fatalError("unreachable!")
    }
}

public func walk<V: Visitor>(v: inout V, stmt: Stmt) {
    switch stmt {
    case is LetStmt:
        let letStmt = stmt as! LetStmt
        if let ty = letStmt.dataType {
            v.visit(type: ty)
        }
        if let e = letStmt.expr {
            v.visit(expr: e)
        }
    case is ExprStmt:
        let expr = stmt as! ExprStmt
        v.visit(expr: expr.expr)
    default: fatalError("unreachable!")
    }
}
public func walk<V: Visitor>(v: inout V, expr: Expr) {
    switch expr {
    case is UnExpr:
        v.visit(expr: (expr as! UnExpr).opnd)
    case is BinExpr:
        let bexpr = expr as! BinExpr
        v.visit(expr: bexpr.lhs)
        v.visit(expr: bexpr.rhs)
    case is CallExpr: do {
        let call = expr as! CallExpr
        v.visit(expr: call.callee)
        call.args.forEach { v.visit(expr: $0) }
    }
    case is TypeParamExpr: do {
        let param = expr as! TypeParamExpr
        v.visit(expr: param.callee)
        param.args.forEach { v.visit(type: $0) }
    }
    case is PathExpr: do {
        let path = expr as! PathExpr
        v.visit(expr: path.lhs)
        v.visit(expr: path.rhs)
    }
    case is DotExpr:
        let dot = expr as! DotExpr
        v.visit(expr: dot.lhs)
        v.visit(expr: dot.rhs)
    case is ConvExpr:
        let conv = expr as! ConvExpr
        v.visit(expr: conv.object)
        v.visit(type: conv.dataType)
    case is LambdaExpr: v.visit(fct: (expr as! LambdaExpr).value)
    case is BlockExpr:
        let block = expr as! BlockExpr
        block.stmts.forEach { v.visit(stmt: $0) }
        
        if let expr = block.expr {
            v.visit(expr: expr)
        }
    case is TemplateExpr:
        let template = expr as! TemplateExpr
        template.parts.forEach { v.visit(expr: $0) }
    case is IfExpr:
        let ifexpr = expr as! IfExpr
        v.visit(expr: ifexpr.cond)
        v.visit(expr: ifexpr.thenBlock)
        if let b = ifexpr.elseBlock {
            v.visit(expr: b)
        }
    case is ForExpr:
        let forExpr = expr as! ForExpr
        v.visit(expr: forExpr.expr)
        v.visit(expr: forExpr.block)
    case is WhileExpr:
        let whileExpr = expr as! WhileExpr
        v.visit(expr: whileExpr.cond)
        v.visit(expr: whileExpr.block)
    case is TupleExpr:
        let tuple = expr as! TupleExpr
        tuple.values.forEach { v.visit(expr: $0) }
    case is ParenExpr: v.visit(expr: (expr as! ParenExpr).expr)
    case is MatchExpr: v.visit(expr: (expr as! MatchExpr).expr)
    case is ReturnExpr:
        let ret = expr as! ReturnExpr
        if let e = ret.expr {
            v.visit(expr: e)
        }
    
    case is BreakExpr, is ContinueExpr: break
    case is SelfExpr, is LitCharExpr, is LitIntExpr: break
    case is LitStrExpr, is LitBoolExpr, is LitFloatExpr, is IdentExpr: break
    case is ErrorExpr: break
    default: fatalError("unreachable!")
    }
}
