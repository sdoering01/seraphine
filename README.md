# Seraphine

Seraphine is a dynamic, strongly typed and interpreted programming language.

Currently Seraphine is a learning and experimentation project for me to explore
the process of transforming human-readable source code into something
executable. That's why its language core is implemented without external
dependencies. Due to the nature of the interpreter being very rudementary,
Seraphine is currently very slow. In numbers, the [sudoku
example](examples/sudoku.sr) is ~2.5 times slower than an equivalent Python
implementation and ~200 times slower than an equivalent Rust implementation on
my machine.

The best way to get a glimpse of a language is an example:

```
// This function returns a new counter object. Since the last expression is
// returned automatically, we can simply use double curly braces.
fn new_counter(initial_count) {{
    count: initial_count,
    previous_counts: [],
    increment() {
        // `this` receiver to access object
        this.previous_counts.push(this.count)
        this.count = this.count + 1
    },
    for_each_count(func) {
        // Boolean coercion
        if (func) {
            // For loops to iterate over lists
            for (c in this.previous_counts) {
                func(c)
            }
        }
    },
    get_initial_count() {
        if (this.previous_counts.length == 0) {
            return this.count
        } else {
            return this.previous_counts[0]
        }
    },
}}

counter = new_counter(42)
println("Counter at", counter.count)
while (counter.count < 45) {
    counter.increment()
}
println("Counter at", counter.count)

// Anonymous functions (closures are possible)
counter.for_each_count(fn (count) {
    println("Counter was", count)
})

println("Initial count was", counter.get_initial_count())
```

More examples can be found in the [examples directory](examples).

## Features

- Arithmetic and logic operations
- If statements with `else if` and `else`
- While loops
    - `break` and `continue`
- Built-in and user-defined functions
    - Automatically return last expression from function
- Types
    - `null`
    - `number`
    - `bool`
    - `string`
    - `function`
    - `list`
    - `object`
    - `iterator`
- Indexing and member access on values
- `this` receiver for object methods
- Reading from stdin and writing to stdout and stderr
- Comments with `//`
- Relaxed parsing of newline characters to allow splitting code into multiple
  lines
- User-friendly error messages, including stack traces
    - *Note*: Stack Traces are currently not available in the REPL
- REPL (read-eval-print loop)

## Execution models

There are two possible ways to execute Seraphine code. There is no practical
use for this. This is just how the language grew -- and since this is a
learning project, I wanted to keep both approaches.

The first approach is the older one: The **Evaluator** directly evaluates the
AST.

The second approach uses the AST to generate bytecode in the form of simple
instructions. The **Virtual Machine (VM)** then runs the instructions. The
bytecode can also be serialized.

> Currently the serialized bytecode also contains the code from which it is
> compiled. This code is only used to produce user-friendly error messages when
> runtime errors occur.

## REPL

The REPL can be used to interactively explore the language and evaluate code.
To start the REPL, pass `repl` as the first argument to the CLI (e.g.:
`seraphine repl`). The REPL currently uses the Evaluator to execute code.

The REPL uses a raw terminal and offers features like:

- Move cursor in current line
- History
- Clear screen
- Evaluation of code that doesn't fit into one line (e.g. if statements with
  non-trivial body)

## Building and running

Since Seraphine is implemented in Rust, you first need to [install the Rust
toolchain](https://www.rust-lang.org/tools/install).

To run the REPL in debug mode, you can use `cargo run -- repl`. You can also
choose to evaluate a file with `cargo run -- eval my_file.sr`. For better
performance you should use the release build by putting the `--release` flag
after `cargo run`. To just build an executable, use `cargo build --release`.
The executable will be placed in the directory `target/release`.

## Testing

All features of Seraphine are covered by tests. Use `cargo test --workspace` to
run the test suite.

## Workspace structure

- `seraphine-core` contains the core functionality (tokenizer, parser,
  Evaluator, bytecode generation und serialization, VM)
- `seraphine-cli` contains the executable for the CLI and REPL functionality
