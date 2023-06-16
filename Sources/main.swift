// The Swift Programming Language
// https://docs.swift.org/swift-book

import Foundation






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
var source = """
pub use collections::{Array, BitSet, BitVec, HashMap, HashSet, Queue, Vec};
pub use traits::{Comparable, Default, Equals, Hash, Iterator, Zero};
pub use primitives::{Bool, Char, Float32, Float64, Int32, Int64, Option, range, Result};
pub use rand::Random;
pub use string::{CodepointIterator, String, StringBuffer, Stringable};
pub use thread::{AtomicInt32, AtomicInt64, Condition, Mutex, Thread};

pub mod collections;
pub mod io;
pub mod primitives;
pub mod rand;
pub mod string;
pub mod thread;
pub mod traits;

pub @internal fn fatalError(msg: String);
pub @internal fn abort();
pub @internal fn exit(status: Int32);
pub @internal fn unreachable[T](): T;

pub fn unimplemented() {
  fatalError("not yet implemented");
}

pub @internal fn print(text: String);
pub @internal fn println(text: String);
pub @internal fn assert(val: Bool);
pub @internal fn debug();
pub @internal fn argc(): Int32;
pub @internal fn argv(idx: Int32): String;
pub @internal fn forceCollect();
pub @internal fn forceMinorCollect();

pub @internal fn timestamp(): Int64;

fn isValidUtf8(data: Array[UInt8]): Bool {
  let mut i = 0i64;

  while i < data.size() {
    let by = data(i).toInt32();
    let mut codepoint = 0i32;
    let mut bytes = 0i32;
    let mut min = 0i32;
    let mut max = 0i32;

    if by & 0x80i32 == 0i32 {
      bytes = 0;
      i = i + 1i64;
      continue;

    } else if by & 0xC0i32 == 0x80i32 {
      return false;

    } else if by & 0xE0i32 == 0xC0i32 {
      codepoint = by & 0x1Fi32;
      bytes = 1;
      min = 0x80;
      max = 0x7FFi32;

    } else if by & 0xF0i32 == 0xE0i32 {
      codepoint = by & 0xFi32;
      bytes = 2i32;
      min = 0x800i32;
      max = 0xFFFFi32;

    } else if by & 0xF8i32 == 0xF0i32 {
      codepoint = by & 0x7i32;
      bytes = 3;
      min = 0x10000;
      max = 0x10FFFFi32;
    }

    while bytes > 0i32 {
      bytes = bytes - 1i32;
      i = i + 1i64;

      if i >= data.size() {
        return false;
      }

      let by = data(i).toInt32();

      if by & 0xC0i32 != 0x80i32 {
        return false;
      }

      codepoint = (codepoint << 6i32) | (by & 0x3Fi32);
    }

    if codepoint < min || codepoint > max {
      return false;
    }

    i = i + 1i64;
  }

  return true;
}

@internal fn unsafeKillRefs[T](arr: Array[T], idx: Int64);

pub @internal fn sleep(seconds: Int32);

pub class Stacktrace {
  backtrace: Option[Array[Int32]],
  elements: Option[Array[StacktraceElement]],
}

impl Stacktrace {
  pub static fn new(): Stacktrace {
    let trace = Stacktrace(None[Array[Int32]], None[Array[StacktraceElement]]);
    trace.retrieveStacktrace();
    trace
  }

  pub fn getStacktrace(): Array[StacktraceElement] {
    if self.elements.isSome() {
      return self.elements.getOrPanic();
    }

    if self.backtrace.isNone() {
      let elements = Array[StacktraceElement]::new();
      self.elements = Some[Array[StacktraceElement]](elements);
      return elements;
    }

    let backtraceLength = self.backtrace.getOrPanic().size();
    let mut i = 0i64;
    let len = backtraceLength / 2i64;
    let elements = Vec[StacktraceElement]::new();
    elements.reserve(len);

    while i < len {
      elements.push(self.getStacktraceElement(i));
      i = i + 1i64;
    }

    let elements = elements.toArray();
    self.elements = Some(elements);
    return elements;
  }

  pub fn printStacktrace() {
    let x = self.getStacktrace();
    let mut i = 0i64;

    for frame in self.getStacktrace() {
      println(frame.toString());
    }
  }

  @internal fn retrieveStacktrace();
  @internal fn getStacktraceElement(idx: Int64): StacktraceElement;
}

class StacktraceElement {
  name: String,
  line: Int32,
}

impl StacktraceElement {
  fn toString(): String {
    "${self.name}: ${self.line}"
  }
}
"""

let src = try? String(contentsOf: URL(fileURLWithPath: "./collections.dora"))

//let res = lex(content: source)
//print(res.errors)
let parser = Parser(with: src ?? "mod a;", interner: &interner)
let ast = parser.parse()
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
