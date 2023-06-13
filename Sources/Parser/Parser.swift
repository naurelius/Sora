



























public final class Parser {
    var tokens: [TokenKind]
    var tokenWidths: [UInt32]
    var tokenIdx: Int
    var idGen: NodeIdGenerator
    var interner: Interner
    let content: String
    var errors: [ParseErrorWithLocation]
    var nodes: [(UInt, UInt32)]
    var offset: UInt32
    
    init(with content: String, interner: inout Interner) {
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

    func parse() -> (File, NodeIdGenerator, [ParseErrorWithLocation]) {}

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
