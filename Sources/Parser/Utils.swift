















public func binarySearch<T: Comparable>(_ a: [T],
 key: T) -> Int? {
     var lowerBound = 0
     var upperBound = a.count
     while lowerBound < upperBound {
         let midIndex = lowerBound + (upperBound -
 lowerBound) / 2
         if a[midIndex] == key {
             return midIndex
         } else if a[midIndex] < key {
             lowerBound = midIndex + 1
         } else {
             upperBound = midIndex
         }
     }
     return nil
 }



public func computeLineStarts(of content: String) -> [UInt32] {
    var pos: UInt32 = 0
    var lineStarts: [UInt32] = [0]
    for ch in content {
        if ch == "\n" {
            lineStarts.append(pos + 1)
        }
        pos += 1
    }
    return lineStarts
}

/*public func computeLineColumn(with lineStarts: [UInt32], offset: UInt32) -> (UInt32, UInt32) {
    let result = binarySearch(lineStarts, key: offset)
    if let result = result {
        if lineStarts[result] == offset {
            guard let idx = UInt32(exactly: result) else { fatalError("Overflow") }
            return (idx + 1, 1)
        }
    }
}*/
