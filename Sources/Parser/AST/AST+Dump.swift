












func dump(_ dumper: AstDumper, _ msg: String) {
    for _ in 0..<dumper.indent*2 {
        print(" ", terminator: "")
    }

    print(msg)
}

/*func dump(dumper: AstDumper, msg: String, span: Span, id: NodeID) {
    for _ in 0..<dumper.indent*2 {
        print(" ", terminator: "")
    }
    print("\(msg) @ \(span) \(id)")
}*/

public func dumpFile(ast: File, interner: Interner) {
    let dumper = AstDumper(interner: interner, indent: 0)
    dumper.dumpFile(file: ast)
} 

public func dumpFct(fct: Function, interner: Interner) {
    let dumper = AstDumper(interner: interner, indent: 0)
    dumper.dumpFct(fct: fct)
}

public func dumpExpr(expr: Expr, interner: Interner) {
    let dumper = AstDumper(interner: interner, indent: 0)
    dumper.dumpExpr(expr)
}

public func dumpStmt(stmt: Stmt, interner: Interner) {
    let dumper = AstDumper(interner: interner, indent: 0)
    dumper.dumpStmt(stmt)
}

final class AstDumper {
    var interner: Interner
    var indent: UInt32

    init(interner: Interner, indent: UInt32) {
        self.interner = interner
        self.indent = indent
    }

    func dumpFile(file: File) {
        for el in file.elements {
            self.dumpElem(el)
        }
    }

    func dumpElem(_ el: Decl) {
        switch el {
        case is Function: dumpFct(fct: el as! Function)
        case is Class: dumpClass(cls: el as! Class)
        case is Struct: dumpStruct(struc: el as! Struct)
        case is Trait: dumpTrait(trait: el as! Trait)
        case is Impl: dumpImpl(impl: el as! Impl)
        case is Global: dumpGlobal(global: el as! Global)
        case is Const: dumpConst(const: el as! Const)
        case is Enum: dumpEnum(en: el as! Enum)
        case is Alias: dumpAlias(alias: el as! Alias)
        case is Module: dumpModule(mod: el as! Module)
        case is Use: dumpUse(use: el as! Use)
        case is ExternPackage: dumpExtern(stmt: el as! ExternPackage)
        case is ErrorDecl: do {
            let decl = el as! ErrorDecl
            dump(self, "error @ \(decl.span) \(decl.id)")
        }
        default: fatalError("unreachable!")
        }
    }

    func dumpGlobal(global: Global) {
        dump(self, "global @ \(global.span) \(global.id)")
        dumpIdent(ident: global.name)
        withIndent { d in 
            d.dumpType(global.dataType)

            if let initialValue = global.initialValue {
                d.dumpExpr(initialValue)
            } else {
                dump(self, "<no value given>")
            }
        }
    }

    func dumpExtern(stmt: ExternPackage) {
        dump(self, "extern package @ \(stmt.span) \(stmt.id)")
        dumpIdent(ident: stmt.name)
        dumpIdent(ident: stmt.identifier)
    }

    func dumpConst(const: Const) {
        dump(self, "const @ \(const.span) \(const.id)")
        dumpIdent(ident: const.name)
        withIndent { d in 
            d.dumpType(const.dataType)
            d.dumpExpr(const.expr)
        }
    }

    func dumpUse(use: Use) {
        dump(self, "use @ \(use.span) \(use.id)")
    }

    func dumpAlias(alias:Alias) {
        dump(self, "alias @ \(alias.span) \(alias.id)")
        dumpIdent(ident: alias.name)

        withIndent { $0.dumpType(alias.type) }
    }

    func dumpModule(mod: Module) {
        dump(self, "module @ \(mod.span) \(mod.id)")
        dumpIdent(ident: mod.name)

        withIndent { d in 
            if let elements = mod.elements {
                elements.forEach { d.dumpElem($0)}
            }
        }
    }

    func dumpEnum(en: Enum) {
        dump(self, "enum @ \(en.span) \(en.id)")
        dumpIdent(ident: en.name)
        withIndent { d in 
            en.variants.forEach { d.dumpEnumValue(value: $0)}
        }
    }

    func dumpEnumValue(value: EnumVariant) {
        dump(self, "enum variant @ \(value.span) \(value.id)")
        dumpIdent(ident: value.name)
        if let types = value.types {
            withIndent { d in 
                types.forEach { d.dumpType($0) }
            }
        }
    }

    func dumpImpl(impl: Impl) {
        dump(self, "impl @ \(impl.span) \(impl.id)")

        withIndent { d in 
            if let traitTy = impl.traitType {
                d.dumpType(traitTy)
                dump(self, "for")
            }
            d.dumpType(impl.extendedType)

            impl.methods.forEach { d.dumpFct(fct: $0) }
        }
    }

    func dumpStruct(struc: Struct) {
        dump(self, "struct @ \(struc.span) \(struc.id)")
        withIndent { d in 
            struc.fields.forEach { d.dumpStructField(field: $0)}
        }
    }

    func dumpStructField(field: StructField) {
        dump(self, "field @ \(field.span) \(field.id)")
        dumpIdent(ident: field.name)
        withIndent { $0.dumpType(field.dataType) }
    }

    func dumpTrait(trait: Trait) {
        dump(self, "trait @ \(trait.span) \(trait.id)")
        withIndent { d in 
            d.dumpIdent(ident: trait.name)
            trait.methods.forEach { d.dumpFct(fct: $0) }
        }
    }

    func dumpClass(cls: Class) {
        dump(self, "class @ \(cls.span) \(cls.id)")
        dumpIdent(ident: cls.name)
        withIndent { d in 
            dump(d, "fields")
            d.withIndent {d in 
                cls.fields.forEach { d.dumpField(field: $0)}
            }
        }
    }

    func dumpAnnotationUsages(anno: AnnotationUsages) {
        for usage in anno.iter() {
            dump(self, "@\(str(name: usage.name)) \(usage.span)")
        }
    }

    func dumpField(field: Field) {
        dump(self, "field @ \(field.span) \(field.id)")
        dumpIdent(ident: field.name)
        withIndent { $0.dumpType(field.dataType) }
    }

    func dumpFct(fct: Function) {
        dump(self, "fct @ \(fct.span) \(fct.id)")
        dumpIdent(ident: fct.name)
        withIndent { d in 
            dump(d, "internal = \(fct.internal)")
            dump(d, "params")
            d.withIndent { d in 
                if fct.params.isEmpty {
                    dump(d, "no params")
                } else {
                    fct.params.forEach { d.dumpParam(param: $0)}
                }
            }

            dump(d, "returns")

            if let ty = fct.returnType {
                d.withIndent { $0.dumpType(ty) }
            } else {
                d.withIndent { dump($0, "<no return type>") }
            }

            dump(d, "executes")
            if let block = fct.block {
                d.withIndent { $0.dumpExpr(block) }
            }
        }

    }

    func dumpParam(param: Param) {
        dump(self, "param @ \(param.span) \(param.id)")
        dumpIdent(ident: param.name)
        withIndent { $0.dumpType(param.dataType) }
    }

    func dumpType(_ type: Type) {
        dump(self, "type @ \(type.span) \(type.id)")
    }

    func dumpStmt(_ stmt: Stmt) {
        switch stmt {
        case is LetStmt: do {
            let letStmt = stmt as! LetStmt
            dumpLetStmt(stmt: letStmt)
        }
        case is ExprStmt: do {
            let exprStmt = stmt as! ExprStmt
            dumpExprStmt(stmt: exprStmt)
        }
        default: fatalError("unreachable!")
        }
    }

    func dumpLetStmt(stmt: LetStmt) {
        dump(self, "let @ \(stmt.span) \(stmt.id)")
        withIndent { d in 
            d.dumpLetPattern(pat: stmt.pattern)
            dump(d, "type")
            d.withIndent { d in 
                if let ty = stmt.dataType {
                    d.dumpType(ty)
                } else {
                    dump(d, "<no type given>")
                }
            }

            dump(d, "expr")
            d.withIndent { d in 
                if let expr = stmt.expr {
                    d.dumpExpr(expr)
                } else {
                    dump(d, "<no expr given>")
                }
            }
        }
    } 

    func dumpLetPattern(pat: LetPattern) {
        switch pat {
        case is IdentPattern: do {
            let ident = pat as! IdentPattern
            dump(self, "ident \(ident.name)")
        }
        case is UnderscorePattern: dump(self, "_")
        case is TuplePattern: do {
            let tuple = pat as! TuplePattern
            dump(self, "tuple")
            self.withIndent {d in 
                tuple.parts.forEach { d.dumpLetPattern(pat: $0) }
            }
        }
        default: fatalError("unreachable!")
        }
    }

    func dumpFor(expr: ForExpr) {
        dump(self, "for @ \(expr.span) \(expr.id)")

        withIndent { d in 
            d.dumpLetPattern(pat: expr.pattern)
            dump(d, "cond")
            d.withIndent { $0.dumpExpr(expr.expr) }
            dump(d, "body")
            d.withIndent { $0.dumpExpr(expr.block) }
        }
    }

    func dumpWhile(expr: WhileExpr) {
        dump(self, "while @ \(expr.span) \(expr.id)")

        withIndent { d in
            dump(d, "cond")
            d.withIndent { $0.dumpExpr(expr.cond) }
            dump(d, "body")
            d.withIndent { $0.dumpExpr(expr.block) }
        }
    }

    func dumpExprStmt(stmt: ExprStmt) {
        dump(self, "expr stmt @ \(stmt.span) \(stmt.id)")
        withIndent { $0.dumpExpr(stmt.expr) }
    }

    func dumpReturn(expr: ReturnExpr) {
        dump(self, "return @ \(expr.span) \(expr.id)")
        withIndent { d in 
            if let expr = expr.expr {
                d.dumpExpr(expr)
            } else {
                dump(d, "<nothing>")
            }
        }
    }

    func dumpBreak(expr: BreakExpr) {
        dump(self, "break @ \(expr.span) \(expr.id)")
    }

    func dumpContinue(expr: ContinueExpr) {
        dump(self, "continue @ \(expr.span) \(expr.id)")
    }

    func dumpExpr(_ expr: Expr) {
        switch expr {
        case is UnExpr: dumpUn(expr: expr as! UnExpr)
        case is BinExpr: dumpBin(expr: expr as! BinExpr)
        case is DotExpr: dumpDot(expr: expr as! DotExpr)
        case is LitCharExpr: dumpLitChar(expr: expr as! LitCharExpr)
        case is LitIntExpr: dumpLitInt(expr: expr as! LitIntExpr)
        case is LitFloatExpr: dumpLitFloat(expr: expr as! LitFloatExpr)
        case is LitStrExpr: dumpLitStr(expr: expr as! LitStrExpr)
        case is TemplateExpr: dumpTemplate(expr: expr as! TemplateExpr)
        case is LitBoolExpr: dumpLitBool(expr: expr as! LitBoolExpr)
        case is IdentExpr: dumpIdentExpr(expr: expr as! IdentExpr)
        case is CallExpr: dumpCall(expr: expr as! CallExpr)
        case is TypeParamExpr: dumpTypeParam(expr: expr as! TypeParamExpr)
        case is PathExpr: dumpPath(expr: expr as! PathExpr)
        case is SelfExpr: dumpSelf(expr: expr as! SelfExpr)
        case is ConvExpr: dumpConv(expr: expr as! ConvExpr)
        case is LambdaExpr: dumpLambda(fct: (expr as! LambdaExpr).value)
        case is BlockExpr: dumpBlock(block: expr as! BlockExpr)
        case is IfExpr: dumpIf(expr: expr as! IfExpr)
        case is TupleExpr: dumpTuple(expr: expr as! TupleExpr)
        case is ParenExpr: dumpParen(expr: expr as! ParenExpr)
        case is MatchExpr: dumpMatch(expr: expr as! MatchExpr)
        case is ForExpr: dumpFor(expr: expr as! ForExpr)
        case is WhileExpr: dumpWhile(expr: expr as! WhileExpr)
        case is BreakExpr: dumpBreak(expr: expr as! BreakExpr)
        case is ContinueExpr: dumpContinue(expr: expr as! ContinueExpr)
        case is ReturnExpr: dumpReturn(expr: expr as! ReturnExpr)
        case is ErrorExpr: dump(self, "error @ \(expr.span) \(expr.id)")
        default: fatalError("unreachable!")
        }
    }

    func dumpBlock(block: BlockExpr) {
        dump(self, "block(\(block.stmts.count) statement(s)) @ \(block.span) \(block.id)")
        withIndent { d in 
            if block.stmts.isEmpty {
                dump(self, "no statements")
            } else {
                block.stmts.forEach { d.dumpStmt($0) }
            }

            if let expr = block.expr {
                dump(self, "value")
                d.dumpExpr(expr)
            }
        }

        dump(self, "block end")
    }

    func dumpIf(expr: IfExpr) {
        dump(self, "if @ \(expr.span) \(expr.id)")
        withIndent { d in 
            d.withIndent { $0.dumpExpr(expr.cond) }
            dump(d, "then")
            d.withIndent { $0.dumpExpr(expr.thenBlock)}
            dump(d, "else")
            d.withIndent { $0.dumpExpr(expr.elseBlock ?? expr.thenBlock) }
        }
    }

    func dumpConv(expr: ConvExpr) {
        withIndent { $0.dumpExpr(expr.object) }
        dump(self, "as @ \(expr.span) \(expr.id)")
        withIndent { $0.dumpType(expr.dataType) }
    }

    func dumpSelf(expr: SelfExpr) {
        dump(self, "self @ \(expr.span) \(expr.id)")
    }

    func dumpLitChar(expr: LitCharExpr) {
        dump(self, "lit char \(expr.value) @ \(expr.span) \(expr.id)")
    }

    func dumpLitInt(expr: LitIntExpr) {
        dump(self, "lit int \(expr.value) @ \(expr.span) \(expr.id)")
    }

    func dumpLitFloat(expr: LitFloatExpr) {
        dump(self, "lit float \(expr.value) @ \(expr.span) \(expr.id)")
    }

    func dumpLitStr(expr: LitStrExpr) {
        dump(self, "lit str \(expr.value) @ \(expr.span) \(expr.id)")
    }

    func dumpTemplate(expr: TemplateExpr) {
        dump(self, "template @ \(expr.span) \(expr.id)")
        withIndent { d in 
            expr.parts.forEach { d.dumpExpr($0) }
        }
    }

    func dumpLitBool(expr: LitBoolExpr) {
        dump(self, "lit bool \(expr.value) @ \(expr.span) \(expr.id)")
    }

    func dumpIdentExpr(expr: IdentExpr) {
        dump(self, "ident \(str(name: expr.name)) @ \(expr.span) \(expr.id)")
    }

    func dumpUn(expr: UnExpr) {
        dump(self, "unary \(expr.op) @ \(expr.span) \(expr.id)")
        withIndent { $0.dumpExpr(expr.opnd) }
    }

    func dumpBin(expr: BinExpr) {
        withIndent { $0.dumpExpr(expr.lhs) }
        dump(self, "binary \(expr.op) @ \(expr.span) \(expr.id)")
        withIndent { $0.dumpExpr(expr.rhs) }
    }

    func dumpLambda(fct: Function) {
        dump(self, "lambda @ \(fct.span) \(fct.id)")
        withIndent { $0.dumpFct(fct: fct) }
    }

    func dumpTuple(expr: TupleExpr) {
        dump(self, "tuple @ \(expr.span) \(expr.id)")
        withIndent { d in 
            for expr in expr.values {
                d.dumpExpr(expr)
            }
        }
    }

    func dumpDot(expr: DotExpr) {
        withIndent { $0.dumpExpr(expr.rhs) }
        dump(self, "dot @ \(expr.span) \(expr.id)")
        withIndent { $0.dumpExpr(expr.rhs) }
    }

    func dumpPath(expr: PathExpr) {
        withIndent { $0.dumpExpr(expr.rhs) }
        dump(self, "path (::) @ \(expr.span) \(expr.id)")
        withIndent { $0.dumpExpr(expr.lhs)}
    }

    func dumpCall(expr: CallExpr) {
        dump(self, "call @ \(expr.span) \(expr.id)")
        withIndent { d in 
            dump(d, "callee")
            d.withIndent { $0.dumpExpr(expr.callee) }

            for arg in expr.args {
                d.dumpExpr(arg)
            }
        }
    }

    func dumpParen(expr: ParenExpr) {
        dump(self, "paren @ \(expr.span) \(expr.id)")
        withIndent {
            $0.dumpExpr(expr.expr)
        }
    }

    func dumpMatch(expr: MatchExpr) {
        dump(self, "match @ \(expr.span) \(expr.id)")

        withIndent { $0.dumpExpr(expr.expr) }
    }

    func dumpTypeParam(expr: TypeParamExpr) {
        dump(self, "type param @ \(expr.span) \(expr.id)")
        withIndent {d in 
            dump(d, "callee")
            self.withIndent { $0.dumpExpr(expr.callee)}
            for arg in expr.args {
                d.dumpType(arg)
            }
        }
    }

    func dumpIdent(ident: Ident?) {
        if let ident = ident {
            dump(self, "ident @ \(str(name: ident.name))")
        } else {
            dump(self, "missing ident")
        }
    }

    func withIndent(_ f: (AstDumper) -> ()) {
        let old = indent
        indent = old + 1
        f(self)
        indent = old
    }

    func str(name: Name) -> String {
        return interner.str(name: name)
    }
}
