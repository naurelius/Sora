import BigInt















public struct TokenSet {
    public let val: BigUInt

    public init(value: BigUInt) {
        self.val = value
    }

    public init(kinds: [TokenKind]) {
        var value: BigUInt = 0
        var i = 0

        while i < kinds.count {
            value |= 1 << (kinds[i].rawValue)
            i += 1
        }
        self.val = value
    }

    public func union(other: TokenSet) -> TokenSet {
        return TokenSet(value: self.val | other.val)
    }

    public func contains(kind: TokenKind) -> Bool {
        return self.val & (1 << kind.rawValue) != 0
    }
}


public enum TokenKind: UInt8 {
    case stringLiteral
    case templateLiteral
    case templateEndLiteral
    case charLiteral
    case intLiteral
    case floatLiteral
    case identifier
    case `true`
    case `false`

    // "big" shapes
    case `class`
    case `enum`
    case `struct`
    case trait
    case impl
    case mod
    case use
    case package
    case extern

    // "small" shapea
    case fn
    case `let`
    case mut
    case const

    // control flow
    case `return`, `if`, `else`, `for`, `while`
    case `in`, `break`, `continue`, match

    //qualifiers
    case this, `super`, `static`, pub

    // casting
    case `as`

    // operators - numbers
    case add, sub, mul, div, modulo

    // operators - logic
    case not, or, caret, and, or_or, and_and

    // operators - comparisons
    case eq_eq, not_eq, eq_eq_eq, not_eq_eq, lt, le
    case gt, ge

    // operators - shifts
    case gt_gt, gt_gt_gt, lt_lt

    // basic syntax
    case eq, comma, semicolon, dot, dot_dot_dot
    case colon, colon_colon, at, arrow, double_arrow

    // brackets
    case lparen, rparen, lbracket, rbracket
    case lbrace, rbrace

    // unused
    case type, alias, capitalThis, underscore

    // trivia
    case whitespace, lineComment, multilineComment

    // unknown character
    case unknown

    // End-of-file. This is the last token
    case eof

    // syntax tree nodes
    case sourceFile

    public func isTrivia() -> Bool {
        switch self {
        case .lineComment, .multilineComment, .whitespace: return true
        default: return false
        }
    }

    public func isEof() -> Bool {
        return self == .eof
    }
}

public let LAST_TOKEN: TokenKind = .eof

public let EXPRESSION_FIRST = TokenSet(kinds: [
    .true,
    .false,
    .stringLiteral,
    .templateLiteral,
    .charLiteral,
    .intLiteral,
    .floatLiteral,
    .identifier,
    .if,
    .match,
    .lbrace,
    .lparen,
    .this,
    .or,
    .or_or,
    .not,
    .add,
    .sub,
    .for,
    .while,
    .break,
    .continue,
    .return
])

extension TokenKind: Equatable, Comparable {
    public static func <(LHS: TokenKind, RHS: TokenKind) -> Bool {
        return LHS.rawValue < RHS.rawValue
    }
}
