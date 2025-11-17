// MIT License
//
// Copyright (c) 2025 Leonard Schütz
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

module charly::grammar

use foo
use foo::bar
use fs::{ File, Foo, Bar }
use net::*
use foo::bar as baz

fn log {}
fn log() {}
fn log(foo: String, bar: Int) {}
fn log(foo: String, bar: Int) -> String {}
fn add(left: Int, vararg right: Int) -> Int = left + right
fn add(left: Int, right: Int) -> Int = left + right
fn constant = 100
fn String.foo = "foo $self"
fn Container<T>.foo = "foo $self"

let foo = 25
let foo: String = foo()
let foo: String?

const foo = 25
const foo: String = foo()
const foo: String?

interface Display {
    fn toString(io: OStream) {}
    fn toString -> String {}
}

interface Foo<T> {}

typealias Foo = Bar
typealias Foo = Foo::Bar
typealias Foo = Foo<String, Int>
typealias Foo = Foo.(Int, Int) -> Int
typealias Foo = Foo<T>.(Int, Int<T>) -> Int<T>
typealias Foo = Foo.<T>(T) -> T
typealias StringMap<V> = Map<String, V>

fn foo {
    return
    return 100

    break

    continue

    defer foo.close()
    defer {
        foo.close()
    }

    for x in mylist do {}
    const squares = x ** 2 for x in data if x > 0

    for (x, index) in mylist.withIndex() do {}

    const squares = {
        const list = []
        for x in data {
            if x > 0 {
                list.push(x ** 2)
            }
        }
        list
    }

    while x < 10 do {}

    loop {}

    {
        let foo = 25
    }

    if x < y then {} else {}
    if x < y then foo else bar
    if x < y then {} else if x > y {} else {}
    const foo = fooBar if x else y

    when { else -> {} }

    when x {
        25 -> foo,
        is Foo -> {},
        is Foo(foo, bar) -> {},
        is Foo<String>(foo, bar) -> {},
        is Ok if x > 10 -> {},
        is Ok(x) if x > 10 -> {},
        is Ok(x, y) if x > y -> {},
        is Ok(x, _) if x > y -> {},
        is Ok(_) if x > y -> {},
        in 10..20 -> {},
        in 10..<20 -> {}
        else -> {},
        else if x < 10 -> {},
    }

    if let Ok(task) = q.read() then {}

    if const Ok(task) = q.read() then {}

    const exprs = [
        await x,
        !x,
        1 + 2,
        0 + 1 * 2 + 3,
        1 || 2 * 2,
        x!!,
        x ?: y ?: z,
        x.y,
        x?.y?.z,
        x(),
        x()(),
        x.y(),
        x() {},
        x.y() {},
        x {},
        x.y {},
        x[1],
        x[1, 2],
        x[1][2],
        (),
        (1),
        (1,),
        (1, 2),
        (1, 2, 3),
        25.25,
        -25.25,
        "hello world",
        "hello $name foo ${1 + 2}",
        "${"$age ${25}"}",
        true,
        false,
        { },
        { || it + 1 },
        { |foo| foo + 1 },
        { |foo, bar| foo + bar },
        { |(left, right)| left + right },
        [],
        [1, 2, [1, 2], [1, 2, 3]],
        .{},
        .{
            "foo": 25,
            "bar": [1, 2, 3],
            "baz": .{
                "foo": 100,
                "bar": 100,
                "baz": 100,
            }
        },
        null,
        0xdeadbeef,
        0xcafebabe,
        0b010101010,
        0o600,
        'a',
        'ä',
        '😀',
    ]

    const foo = if x then y else z
    const bar = when x {
        0 -> false,
        1 -> true,
    }
    const task = select {
        read task from ch1 -> {
            ok -> task
            else -> null
        }
    }

    let foo = "hello"
    foo = "world"

    let bar = 100
    bar += 1
    bar -= 1
    bar *= 2
    foo.bar += 1
    foo().bar += 1
    arr[i] *= 3

    const incl = 20..30
    const excl = 20..<30
    const foo = data[1..-1]

    const (left, right) = getPair()
    const { name, age } = person

    const foo = Color::RGB(0, 0.5, 1)

    const evenPositiveNumbersSquaredSummed = numbers
        .filter { it > 0 && it % 2 == 0 }
        .map { it ** 2 }
        .sum()

    const evenPositiveNumbersSquaredSummed = numbers
        .filter { |num| num > 0 && num % 2 == 0 }
        .map { |num| num ** 2 }
        .sum()

    const names = users.map &.name

    containers.each &.lock()
}

struct Foo<T> {
    let foo: String
    const bar: String
    private let bar: String
    private const bar: String
}

impl Foo<T> {
    fn foo {}
    private fn foo {}
}

impl MyInterface for Foo<T> {
    fn foo {}
    private fn foo {}
}

struct Foo<T: Iterable<U> & Clone, U> {}

interface MyInterface {
    fn foo {}
}

interface Foo<T> {}

struct Foo {
    companion object {
        let foo: String = 200
        const bar: String = 100
        private let foo: String = 200
        private const bar: String = 100
        fn foo {}
        private fn foo {}
    }
}

object Foo {
    let foo: String = 200
    const bar: String = 100
    private let foo: String = 200
    private const bar: String = 100
    fn foo {}
    private fn foo {}
}

struct Foo {
    struct Bar {}
    enum Bar {}
    interface Bar {}
    typealias Foo = Bar
}

interface Foo {
    struct Bar {}
    enum Bar {}
    interface Bar {}
    typealias Foo = Bar
}

struct Map<K, V> {
    typealias Node = (key: K, value: V)
    typealias NodeBin = List<Node>
}

enum Result<T, E> {
    Some(T),
    Err(E)
}

enum Option<T> {
    Some(T),
    None
}

enum Color {
    RGB(red: Float, green: Float, blue: Float),
    HSV(hue: Float, saturation: Float, value: Float),
    Gray(value: Float),
}

impl Color {
    fn toRGB -> Color::RGB {}
}

impl Display for Color {}

export let foo: Int = 25
export const foo: Int = 25
export fn foo() {}
export typealias Foo = String
export struct Foo {}
export interface Foo {}
export enum Foo {}

// ------ Defer ------
defer file.close()
defer {
    file.close()
}

// ------ For ------
for x in data do println(x)
for x in data do {
    println(x)
}

// ------ While ------
while x do println(x)
while x do {
    println(x)
}
while let Some(task) = task.read() do handle(task)
while let Some(task) = task.read() do {
    handle(task)
}

// ------ Loop ------
loop println("hello world")
loop {
    println("hello world")
}

// ------ If ------
if x do println("hello world")
else println("goodbye world")

if x do { println("hello world") }
else { println("goodbye world") }
