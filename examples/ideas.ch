//let foo = 100
//let foo = foo
//let foo = (100)
//let foo = (foo)
//let foo = foo()
//let foo = foo(1, 2, foo(3, 4))
//let foo = 10 + 20 * 30 + 40
//let a = 10 + 20 * 30 > 100 == false && true || false
let a = foo(10, 20, bar(30) * 2) * 2



const valuePrinter = { value ->
    println(value)
}

repeat(100) { valuePrinter(it) }
repeat(100, valuePrinter)

struct ValuePrinter(
    val title: String
) {
    fn call(value: Any) {
        println("$title: value")
    }
}

const printer = ValuePrinter("log")

repeat(100, printer.call)

enum ThreatLevel {
    None,
    Low,
    Medium,
    High,
    Extreme
}

export const defaultPort = 3001
export const defaultHost = "127.0.0.1"

export let currentThreatLevel = ThreatLevel.Low

const ch = Channel<Int>(8)
const (result, ok) = ch.read()
const ok = ch.write(100)

const exc = Exception("some exception")

use runtime::fiber::sleep

const f1 = spawn {
    sleep(2.seconds)
    exc.updateStackTrace()
    throw exc
}

const f2 = spawn {
    await f1
}

const f3 = spawn {
    await f1
}

const e1 = await f2 catch it
const e2 = await f3 catch it

assert e1 === e2
assert e1 === exc
assert e2 === exc



const ch1 = Channel<String>(0)
const ch2 = Channel<String>(0)

spawn {
    sleep(1.second())
    ch2.write("hello world")
}

select {
    ch1.read() -> { value, ok ->
        // ch1 was available first
    }

    ch2.read() -> { value, ok ->
        // ch2 was available first
        // in this example, this will execute
    }
}

const entry: Map<String>::Entry? = null

typealias MyFunc = (Int, ...Int) -> Int

const f: MyFunc = { initial, ...values ->
    let sum = initial
    for n in values sum += n
    return sum
}

fn accept(initial: Int, numbers: List<Int>, block: MyFunc) -> Int {
    return block(initial, ...numbers)
}

accept(0, [1, 2, 3, 4], f)

const q = Channel<Task>(-1)

spawn {
    repeat(10) {
        q.send(Task())
    }
    q.close()
}

enum <R, E> Result {
    Ok(value: R)
    Err(error: E)
}

loop {

    // source
    select {
        q.read() -> { result ->
            const task = result.unwrap() catch break
        }

        time.after(1.second()) -> {
            break
        }

        else -> {
            println("none of the above were ready")
        }
    }

    // desugared
    {
        enum __SelectChoice {
            Else,
            Case(index: Int, result: Any)
        }
        const __select_cases: Tuple<Future<Task, Exception>, Future<Unit, Exception>> = (q.read(), time.after(1.second()))
        const __select_choice: __SelectChoice = runtime::fiber::select_impl(__select_cases, /*has_else=*/true)
        when __select_choice {
            Case(_, __select_result) if __select_choice.index == 0 -> {
                const result = __select_result as Result<Task, Exception>
                {
                    const task = result.unwrap() catch break
                }
            }

            Case(_, __select_result) if __select_choice.index == 1 -> {
                const it = __select_result as Result<Unit, Exception>
                {
                    break
                }
            }

            Else -> {
                println("none of the above were ready")
            }
        }
    }

    // desugared
    {
        enum __SelectChoice {
            Else,
            Case(index: Int, result: Any)
        }
        const __select_cases: Tuple<Future<Task, Exception>, Future<Unit, Exception>> = (q.read(), time.after(1.second()))
        const __select_choice: __SelectChoice = runtime::fiber::select_impl(__select_cases, /*has_else=*/true)
        {
        cond0:
            if (__select_choice is __SelectChoice.Case) {
                const __select_index = __select_choice.index
                const __select_result = __select_choice.result

                if (__select_index == 0) {
                    const result = __select_result as Result<Task, Exception>
                    {
                        const task = result.unwrap() catch break
                    }
                } else goto cond1
            }

        cond1:
            if (__select_choice is __SelectChoice.Case) {
                const __select_index = __select_choice.index
                const __select_result = __select_choice.result

                if (__select_index == 1) {
                    const it = __select_result as Result<Unit, Exception>
                    {
                        break
                    }
                } else goto cond2
            }

        cond2:
            if (__select_choice is __SelectChoice.Else) {
                println("none of the above were ready")
            }
        }
    }
}

const result = result.unwrap() if result.isOk() else break
const result = result.unwrap() catch break
const emails = user.email for user in service.users
