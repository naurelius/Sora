





let keywords: [String: TokenKind] = [
    "true": .true,
    "false": .false,
    "class": .class,
    "enum": .enum,
    "struct": .struct,
    "trait": .trait,
    "impl": .impl,
    "mod": .mod,
    "use": .use,
    "package": .package,
    "fn": .fn,
    "let": .let,
    "mut": .mut,
    "const": .const,
    "return": .return,
    "if": .if,
    "else": .else,
    "while": .while,
    "for": .for,
    "in": .in,
    "break": .break,
    "continue": .continue,
    "match": .match,
    "self": .this,
    "super": .super,
    "pub": .pub,
    "static": .static,
    "as": .as,
    "type": .type,
    "alias": .alias,
    "Self": .capitalThis
]


public struct LexerResult {
    public var tokens: [TokenKind]
    public var widths: [UInt32]
    public var errors: [ParseErrorWithLocation]
}

public func lex(content: String) -> LexerResult {
    let lexer = Lexer(with: content)
    var tokens: [TokenKind] = []
    var widths: [UInt32] = []

    while !lexer.isEof {
        let start = lexer.offsetu32
        let token = lexer.readToken()
        assert(token < TokenKind.eof)
        let end = lexer.offsetu32
        tokens.append(token)
        widths.append(end - start)
    }
    return LexerResult(tokens: tokens, widths: widths, errors: lexer.errors)
}

public final class Lexer {
    var content: String
    var offset: String.Index
    var errors: [ParseErrorWithLocation]
    var openBraces: [UInt]

    public init(with content: String) {
        self.content = content
        self.offset = content.startIndex
        self.errors = []
        self.openBraces = []
   }

   func readToken() -> TokenKind {
       guard let ch = self.curr else { fatalError("end of file reached" ) }
       if ch.isWhitespace {
           return self.readWhiteSpace()
       } else if ch.isDecDigit {
           return self.readNumber()
       } else if self.isLineComment {
           return self.readLineComment()
       } else if self.isMultilineComment {
           return self.readMultilineComment()
       } else if ch.isIdentifierStart {
           return self.readIdentifier()
       } else if ch.isQoute {
           return self.readString(continuation: false)
       } else if ch.isCharQuote {
           return self.readCharLiteral()
       } else if ch.isOperator {
           return self.readOperator()
       } else {
           return self.readUnknownChar()
       }
    }

   func readUnknownChar() -> TokenKind {
       let start = self.offsetu32
       guard let ch = self.curr else { fatalError("missing char") }

       self.eatChar()
       let span = self.spanFrom(start: start)
       self.reportErrorAt(msg: .unknownChar(ch), span: span)
       return .unknown
   }

   func readWhiteSpace() -> TokenKind {
       while let ch = self.curr, ch.isWhitespace {
           self.eatChar()
       }
       return .whitespace
   }

   func readLineComment() -> TokenKind {
       while let ch = self.curr, !ch.isNewline {
           self.eatChar()
       }
       return .lineComment
   }

    func readMultilineComment() -> TokenKind {
        let start = self.offsetu32

        self.eatChar()
        self.eatChar()

        while self.curr != nil && !self.isMultilineCommentEnd {
            self.eatChar()
        }
        if self.curr == nil {
            let span = self.spanFrom(start: start)
            self.reportErrorAt(msg: .unclosedComment, span: span)
        }
        self.eatChar()
        self.eatChar()

        return .multilineComment
    }

    func readIdentifier() -> TokenKind {
        let value = self.readIdentifierAsString()

        let lookup = keywords[value]

        if let tokType = lookup {
            return tokType
        } else if value == "_" {
            return .underscore
        } else {
            return .identifier
        }
    }

    func readIdentifierAsString() -> String {
        var value = String()

        while let ch = self.curr, ch.isIdentifier {
            self.eatChar()
            value.append(ch)
        }
        return value
    }

    func readCharLiteral() -> TokenKind {
        let start = self.offsetu32
        self.eatChar()

        while let curr = self.curr, !curr.isCharQuote {
            self.readEscapedChar()
        }
        if let curr = self.curr, curr.isCharQuote {
            self.eatChar()
        } else {
            let span = self.spanFrom(start: start)
            self.reportErrorAt(msg: .unclosedChar, span: span)
        }
        return .charLiteral
    }

    func readEscapedChar() {
        if self.eatChar() == "\\" {
            self.eatChar()
        }
    }

    func readString(continuation: Bool) -> TokenKind {
        var start = self.offsetu32

        if continuation {
            start -= UInt32("}".utf8.count)
        } else {
            assert(self.curr == "\"")
            self.eatChar()
        }
        while let curr = self.curr, !curr.isQoute {
            if self.curr == "$" && self.lookahead == "{" {
                self.eatChar()
                self.eatChar()
                self.openBraces.append(1)
                return .templateLiteral
            }
            self.readEscapedChar()
        }
        
        if let curr = self.curr, curr.isQoute {
            self.eatChar()
        } else {
            let span = self.spanFrom(start: start)
            self.reportErrorAt(msg: .unclosedString, span: span)
        }

        if continuation {
            return .templateEndLiteral
        } else {
            return .stringLiteral
        }

    }

    func readOperator() -> TokenKind {
        let ch = self.curr!
        self.eatChar()

        let nch = self.curr ?? "x"
        let nnch = self.lookahead ?? "x"

        switch ch {
        case "+": return .add
        case "-": do {
            if nch == ">" {
                self.eatChar()
                return .arrow
            } else {
                return .sub
            }
        }
        case "*": return .mul
        case "/": return .div
        case "%": return .modulo

        case "(": return .lparen
        case ")": return .rparen
        case "[": return .lbracket
        case "]": return .rbracket
        case "{": do {
            if var openBracesTop = self.openBraces.last {
                openBracesTop += 1
            }
            return .lbrace
        }
        case "}": do {
            if var openBracesTop = self.openBraces.last {
                openBracesTop -= 1
                if openBracesTop == 0 {
                    _ = self.openBraces.popLast()
                    return self.readString(continuation: true)
                }
            }
            return .rbrace
        }
        case "|": do {
            if nch == "|" {
                self.eatChar()
                return .or_or
            } else {
                return .or
            }
        }
        case "&": do {
            if nch == "&" {
                self.eatChar()
                return .and_and
            } else {
                return .and
            }
        }
        case "^": return .caret
        case ",": return .comma
        case ";": return .semicolon
        case ":": do {
            if nch == ":" {
                self.eatChar()
                return .colon_colon
            } else {
                return .colon
            }
        }
        case ".": do {
            if nch == "." && nnch == "." {
                self.eatChar()
                self.eatChar()
                return .dot_dot_dot
            } else {
                return .dot
            }
        }
        case "=": do {
            if nch == "=" {
                self.eatChar()
                if nnch == "=" {
                    self.eatChar()
                    return .eq_eq_eq
                } else {
                    return .eq_eq
                }
            } else if nch == ">" {
                self.eatChar()
                return .double_arrow
            } else {
                return .eq
            }
        }
        case "<": do {
            switch nch {
            case "=": do {
                self.eatChar()
                return .le
            }
            case "<": self.eatChar(); return .lt_lt
            default: return .lt
            }
        }
        case ">": switch nch {
        case "=": self.eatChar(); return .ge
        case ">": do {
            self.eatChar()
            if nnch == ">" {
                self.eatChar()
                return .gt_gt_gt
            } else {
                return .gt_gt
            }
        }
        default: return .gt
        }
        case "!": if nch == "=" {
            self.eatChar()
            if nnch == "=" {
                self.eatChar()
                return .not_eq_eq
            } else {
                return .eq_eq
            }
        } else {
            return .not
        }
        case "@": return .at
        default: fatalError("unreachable!")
        }
    }

    func readNumber() -> TokenKind {
        let base: Int
        if self.curr == "0" {
            let next = self.lookahead
            switch next {
            case "x": do {
                self.eatChar()
                self.eatChar()
                base = 16
            }
            case "b": do {
                self.eatChar()
                self.eatChar()
                base = 2
            }
            default: base = 10
            }
        } else {
            base = 10
        }
        
        self.readDigits(base: base)
        if base == 10 && self.curr == ".", let ch = self.lookahead, ch.isDecDigit {
            return self.readNumberAsFloat()
        }

        if let ch = self.curr, ch.isIdentifierStart {
            self.readIdentifierAsString()
        }

        return .intLiteral
    }

    func readNumberAsFloat() -> TokenKind {
        self.eatChar()
        self.readDigits(base: 10)
        if self.curr == "e" || self.curr == "E" {
            self.eatChar()
            if self.curr == "+" || self.curr == "-" {
                self.eatChar()
            }
            self.readDigits(base: 10)
        }
        if let curr = self.curr?.isIdentifierStart, curr {
            self.readIdentifierAsString()
        }
        return .floatLiteral
    }

    func spanFrom(start: UInt32) -> Span {
        return Span(start: start, len: self.offsetu32 - start)
    }

    func readDigits(base: Int) {
        while let curr = self.curr?.isDigit(base: base), curr {
            self.eatChar()
        }
    }

    @discardableResult func eatChar() -> Character? {
        if let curr = self.curr {
            self.offset = self.content.index(after: self.offset)
            return curr
        } else {
            return nil
        }
    }

    var offsetu32: UInt32 {
        UInt32(self.offset.utf16Offset(in: self.content))
    }

    var isEof: Bool {
        self.offset == self.content.endIndex
    }

    var curr: Character? {
        guard self.offset < self.content.endIndex else { return nil }
        var iter = self.content[self.offset...].makeIterator()
        return iter.next()
    }

    var lookahead: Character? {
        guard self.offset < self.content.endIndex else { return nil }
        var iter = self.content[self.offset...].makeIterator()
        _ = iter.next()
        return iter.next()
    }

    func reportErrorAt(msg: ParseError, span: Span) {
        self.errors.append(ParseErrorWithLocation(span: span, error: msg))
    }

    var isLineComment: Bool {
        self.curr == "/" && self.lookahead == "/"
    }

    var isMultilineComment: Bool {
        self.curr == "/" && self.lookahead == "*"
    }
    
    var isMultilineCommentEnd: Bool {
        self.curr == "*" && self.lookahead == "/"
    }
}


extension Character {
 /// Indicates whether `self` character represents a decimal digit.
  fileprivate var isDecDigit: Bool {
    guard let ascii = asciiValue else { return false }
     return (0x30 ... 0x39) ~= ascii  // 0 ... 9
     //|| 0x5f == ascii  // _
    }

 /// Indicates whether `self` represents an hexadecimal digit.
 fileprivate var isHexDigit: Bool {
    guard let ascii = asciiValue else { return false }
    return (0x30 ... 0x39) ~= ascii  // 0 ... 9
    || (0x41 ... 0x46) ~= ascii  // A ... F
    || (0x61 ... 0x66) ~= ascii  // a ... f
    //|| 0x5f == ascii  // _
 }

 /// /// Indicates whether `self` represents an octal digit.
  fileprivate var isOctDigit: Bool {
    guard let ascii = asciiValue else { return false }
     return (0x30 ... 0x37) ~= ascii  // 0 ... 7
    //|| 0x5f == ascii  // _
    }

 /// Indicates whether `self` represents a binary digit.
 fileprivate var isBinDigit: Bool {
    self == "0" || self == "1" //|| self == "_"
 }

 /// Indicates whether `self` represents an operator.
 fileprivate var isOperator: Bool {
 "^+-*/%&|,=!;:.()[]{}<>@".contains(self)
  }

  fileprivate func isDigit(base: Int) -> Bool {
      let result: Bool
      switch base {
      case 2: result = isBinDigit
      case 8: result = isOctDigit
      case 10: result = isDecDigit
      case 16: result = isHexDigit
      default: return false;
      }
      return result || self == "_"
  }

  fileprivate var isQoute: Bool {
      self == "\""
  }

  fileprivate var isCharQuote: Bool {
      self == "'"
  }

  fileprivate var isIdentifierStart: Bool {
      guard let ascii = asciiValue else { return false }
      return (0x61...0x7a) ~= ascii || (0x41...0x5a) ~= ascii || 0x5f == ascii
  }

  fileprivate var isIdentifier: Bool {
      isIdentifierStart || isDecDigit
  }

}

