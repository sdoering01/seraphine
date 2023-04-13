# Rust Calculator

This project can be used to evaluate arithmetic expressions like
`-(2 + 2 * 3 ^ 5) / 8`. I made this project to experiment with tokenizers, ASTs
and the evaluation of those.

The goal is to replicate some or all high-level functionality of GNU
[bc](https://www.gnu.org/software/bc/). However, achieving full compatibility
with [POSIX bc](https://pubs.opengroup.org/onlinepubs/9699919799/) is not a
goal.

## Features

- [x] Basic unary operators: `+`, `-`
- [x] Basic binary operators: `+`, `-`, `*`, `/`, `%`, `^`
- [x] Brackets
- [x] Variables (`a = 2`, `b = 2`, `c = a + b`)
- [ ] Support for arbitrary precision integers (can currently only handle signed 64-bit integers)
- [ ] Support for (arbitrary precision) floating point values
- [x] Built-in variables (`e`, `pi`, ...)
- [ ] Built-in functions (`sin`, `cos`, `log`, ...)
- [ ] Handle user input (e.g. evaluate lines from a file and stdin)
- [ ] Interactive terminal REPL

Depending on how much time I am going to invest into this project:

- [ ] User-provided functions
- [ ] Representation of boolean-like variables
- [ ] Logical NOT operator (`!`)
- [ ] Comparison operators (`<`, `>`, `==`, ...)
- [ ] If statements
- [ ] While loops
- [ ] ... and thus Turing-completeness (although one could argue that through recursion the while loops wouldn't be required for this)
