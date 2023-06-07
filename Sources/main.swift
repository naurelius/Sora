// The Swift Programming Language
// https://docs.swift.org/swift-book






while !lex.isEof {
    print(lex.offsetu32)
    //lex.readDigits(base: 10)
    print(lex.readNumber())
    lex.eatChar()
    //lex.eatChar()
}
print("Hello, world!")
