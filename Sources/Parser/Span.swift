



public struct Span: CustomStringConvertible {
    public let start: UInt32
    public let len: UInt32

    public init(start: UInt32, len: UInt32) {
        self.start = start
        self.len = len
    }

    public static func at(start: UInt32) -> Span {
        return Span(start: start, len: 0)
    }

    public var end: UInt32 {
        self.start + self.len
    }

    public var description: String {
        return "\(self.start)-\(self.end)"
    }
}

extension Span: Equatable {}

