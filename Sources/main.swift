// The Swift Programming Language
// https://docs.swift.org/swift-book








let result = lex(content: """
                 // a simple function
                 @test
                 fn do() -> void {
                 +=-_|:[]{}
                 123456
                 && || == => << >>> >> <> 
                 "Hello World"
                 'h' / 'e'
                 mod mut let Self self const class

                 enum A {
                 A, B, C
                 }
                 ~#?
                 }
                 /*
                 * a multiline comment
                 */
                 let i: String = ""
                 """)
print(result.tokens, result.errors.map { return "\($0.span), \($0.error.message)"}.joined(separator: ", "))
print("Hello, world!")
