import Foundation









public enum ParseError: Error, Equatable {
    // Lexer errors
    case unknownChar(Character)
    case unclosedComment
    case unclosedString
    case unclosedChar
    case invalidEscapeSequence

    // Parser errors
    case expectedTopLevelDeclaration
    case unknownAnnotation(String)
    case redundantAnnotation(String)
    case misplacedAnnptation(String)
    case expectedToken(String)
    case expectedType
    case misplacedElse
    case expectedFacor
    case numberOveflow
    case unclosedStringTemplate
    case expectedIdentifier
    case invalidSuffix(String)
    case expectedExpression
    case expectedImplElement

    public var message: String {
        switch self {
            case .unknownChar(let ch): return "unknown character \(ch) (Codepoint) \(ch.unicodeScalars.first!.value)."
        case .unclosedComment: return "unclosed comment."
        case .unclosedString: return "unclosed string."
        case .unclosedChar: return "unclosed char."
        case .invalidEscapeSequence: return "unknown escape sequence."
        // Parser errors
        case .expectedTopLevelDeclaration: return "expected top-level declaration."
        case .misplacedAnnptation(let modifier): return "mislpaced annotation \(modifier)."
        case .redundantAnnotation(let token): return "redundant annotation \(token)."
        case .unknownAnnotation(let token): return "unknown annotation \(token)."
        case .expectedToken(let token): return "expected `\(token)`."
        case .numberOveflow: return "number too large to be represented."
        case .expectedType: return "type expected."
        case .misplacedElse: return "misplaced else."
        case .expectedFacor: return "factor expected."
        case .unclosedStringTemplate: return "unclosed string template."
        case .expectedIdentifier: return "identifier expected."
        case .invalidSuffix(let suffix): return "invalid suffix `\(suffix)`."
        case .expectedExpression: return "expected expression."
        case .expectedImplElement: return "expected impl element (function)."
        }
    }
}

public struct ParseErrorWithLocation: Error {
    public private(set) var span: Span
    public private(set) var error: ParseError
}
