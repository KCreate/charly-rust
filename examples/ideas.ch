enum Result<R, E> {
    Ok(R),
    Err(E)
}

enum Color {
    RGB(r: Int, g: Int, b: Int),
    HSV(hue: Int, sat: Int, val: Int),
    Grayscale(val: Int)

}

impl Color {
    fn toRGB -> (Int, Int, Int) {
        match self {
            is RGB => (self.r, self.g, self.b),
            is HSV => (?, ?, ?),
            is Grayscale => .........
        }
    }
}

struct Person {
    let name: String
    let age: Int32
    let hobbies: List<string>
}

interface FooBar {
    fn doStuff -> Int32
}

impl Person {
    fn doStuff -> String { ... }
}

impl FooBar for Person {
    fn doStuff -> Int32 { ... }
}

select {

    // with explicit result value
    when result = channel.read() => {}
    when result = channel.send(value) into result => { ... }
    when result = await future into result => { ... }

    // with implicit result value
    when channel.read() => match it {  }
    when channel.send(value) => match it { ... }
    when await future => match it { ... }

    else => { ... }
}
// import syntax
import std.foo.bar.*
import std.foo.bar.File
import std.foo.bar.{ Foo, Bar, Baz }
import std.foo.bar.File as MyFile
import std.foo.bar.{
    Foo as MyFoo,
    Bar as MyBar,
    Baz as MyBaz,
}

// will import the File type from the package std.foo.bar
// the package must be located at <import path>/std/foo/bar
// the directory a file is located in determines the package it is in
import std.foo.bar.File

private struct Foo {
    let value: String
}

export struct Bar {
    let value: Foo
}

foo = List<String>("hello world")
foo = foo<String>("hello world")

users.map &.into<String>()

users.map { it.into<String>() }


enum CSTNode {
    Tree(type: CSTKind, children: List<CSTNode>),
    Token(type: TokenKind, text: String, location: Location)
}

impl CSTNode {
    fn location(self) -> Location {
        return match self {
            is Token(_, _, location) => location
            is Tree(_, children) => {
                assert children.size > 0
                return Location.merge(
                    children.first().location(),
                    children.last().location(),
                )
            }
        }
    }
}

struct Box<T> {
    const value: T
}

impl<T> Box<T> {
    #[IR(AllowConstAssignment)]
    fn constructor(value: T) {
        self.value = value
    }
}

impl<T> Iterable<T> for Box<T> {
    fn iterator(self) = Iterator<T>.fromList([self.value])
}

const myLambda: |String, String| -> String = { |left: String, right: String| -> string}

let x: Any = get()

match x {
    is Person { name, age } => {}
    is Color(255, 0, 0) => {}
    is Color(0, 255, 0) => {}
    is Color(0, 0, 255) => {}
    is Color(r, g, b) => {}
}

const adultSwissUserEmails = server.getUsers()
    .filter -> |x, y| {
         it.age >= 18
     }
    .filter -> { |x, y|
        it.region == "CH"
    }
    .map -> { it.email }

if users.any -> it.age < 18 {

}

const foo = ->|value: String|: String value.length
const foo = ->|value: String|: String { value.length }
const foo = ->|value| value.length
const foo = ->|value| { value.length }
const foo = ->it.length
const foo = ->{ value.length }

// match unpack expressions
match x {

    // enum unpack
    is Ok => { ... }
    is Color(r, g, b) => { ... }
    is Point(0, y, z) => { ... }
    is Point(x, 0, z) => { ... }
    is Point(x, y, 0) => { ... }
    is Point(x, y, z) => { ... }

    // tuple unpack and match
    (100, name, age) => { ... }
    else => {}
}

// field unpack
const { id, username, email } = request.user

// tuple unpack
let (a, b, c) = (1, 2, 3)
let (..., b, c) = (1, 2, 3)
let (a, ..., c) = (1, 2, 3)
let (a, b, ...) = (1, 2, 3)

// if / while typecheck
if let Ok = value {}
if let Ok(a, b, c) = value {}
while let Ok = value {}
while let Ok(a, b, c) = value {}

// typecast syntax
foo.bar as List<String>
foo.bar as? List<String>

#[key]
#[key = value]
#[key(key = value, value)]
fn foo {}








union Result<T, E> {
    Ok(value: T),
    Err(error: E)
}

enum Priority {
    Low,
    Medium,
    High,
}

enum WellKnownOffsets : i32 {
    HeaderPtr = 0x00,
    ConstantPoolPtr = 0x08,
    FunctionPoolPtr = 0x10,
}

enum flags Roles : i32 {
    Client,
    Salesperson,
    Technician,
    Manager,
    Owner,
}

// <author>.<project>.<package>.<module-type-path>
import std.io
import std.text.Encoding
import kcreate.utils.csv.*
import kcreate.utils.json.*
import kcreate.utils.json.*




"hello world"
StringStart                     "
StringTextPart                  hello world
StringEnd                       "

"hello { if foo {} }!"
StringStart                     "
StringTextPart                  hello
StringExprStart                 {
Identifier                      name
StringExprEnd                   }
StringTextPart                  !
StringEnd                       "



struct List<T> {
    let buffer: UninitializedArray<T>
    let size: i32
}

@WellKnownType
enum ControlFlow {
    Return(R),
    Continue,
    Break,
    BreakWithValue(value: R)
}

impl<T> List<T> {
    func map<R>(self, transformer: |value: T, index: i32| -> R) -> List<R> {
        const result = List<R>.withCapacity(self.size)
        for i in 0..self.size {
            result.add(transformer(self.at(i), i))
        }
        return result
    }

    func each(self, action: |value: T, index: i32| -> ControlFlow<()>) {
        for i in 0..self.size {
            action(self.at(i), i)
        }
    }
}

const nonEvenNumbersDoubled = data.map -> |e, i| {
    if i % 2 == 0 continue
    e * 2
}

const data: List<i32> = getData()
const result = data
    .map -> { it * 2 }
    .filter -> { it % 3 == 0 }
    .any -> { it > 10 }





@Deprecated(
    message = "Use add(a:b:) instead",
    since = "1.2.3",
    reason = "This function is deprecated due to performance issues.",
    removalVersion = "2.0.0"
)
@NeverInline
@NoMangle
@MyCustomAnnotation
@FooBarLevel(42)
func add(a: i32, b: i32) -> i32 {
    return a + b
}







// end of file
