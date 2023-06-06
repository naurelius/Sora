import Foundation


public struct Name: Equatable, Hashable {
    public let val: UInt
}

public struct Interner {
    public let lock: NSLock = .init()
    var data: Internal

    public init() {
        self.data = .init(map: [:], vec: [])
    }

    public mutating func intern(name: String) -> Name {
        self.lock.lock()
        defer {self.lock.unlock()}
        if let val = self.data.map[name] {
            return val
        }
        let key = name
        let value = Name(val: UInt(data.vec.count))
        data.vec.append(key)
        data.map[key] = value

        return value
    }

    public func str(name: Name) -> String {
        self.lock.lock()
        defer {self.lock.unlock()}
        return .init(data.vec[Int(name.val)])
    }
}

struct Internal {
    public var map: [String: Name]
    public var vec: [String]
}

