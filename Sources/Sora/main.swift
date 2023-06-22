// The Swift Programming Language
// https://docs.swift.org/swift-book

import Foundation
import Parser






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
                 "Hi ${name}"
                 }
                 /*
                 * a multiline comment
                 */
                 let i: String = ""
                 """)
var interner = Interner()
var source = "class A(i: Int, j: Int)"

//let src = try? String(contentsOf: URL(fileURLWithPath: "./collections.dora"))

//let res = lex(content: source)
//print(res.errors)
let parser = Parser(with: source, interner: &interner)
let ast = parser.parse()
print(ast.errors)
//print(ast.errors)
//ast.errors.forEach { e in 
  //  let span = e.span
  //  let str = source[source.index(source.startIndex, offsetBy: Int(span.start - 10))..<source.index(source.startIndex, offsetBy: Int(span.end))]
    //print(str)
//}
//print(ast.ast, ast.errors)
dumpFile(ast: ast.ast, interner: parser.interner)
//dumpFile(ast: a, interner: Interner())
print("Hello, world!")
