# SmallC Interpreter | OCaml

A miniature **C-like language interpreter** built in OCaml with a hand-written lexer, a recursive-descent parser, and an AST evaluator. Instead of stopping at tokenization or syntax checking, this project connects all three stages to execute programs with variables, expressions, loops, branches, and function calls.

> Developed through **University of Maryland coursework**. The language specification, some data types, the command-line driver, and utility scaffolding were provided as starter material. The lexer, parser, and evaluator include my implementation work.

## How the interpreter works

```text
SmallC source (.c)
        |
        v
1. Lexer (src/lexer.ml)
   characters -> tokens
        |
        v
2. Parser (src/parser.ml)
   tokens -> abstract syntax tree (AST)
        |
        v
3. Evaluator (src/eval.ml)
   AST + execution environment
        |
        v
Program output / updated state
```

### 1. Lexing — source to tokens

The lexer walks the source one character at a time. It scans integer literals and identifiers, recognizes keywords (`int`, `bool`, `if`, `else`, `while`, `for`, `return`, `printf`), and matches compound operators such as `==`, `!=`, `>=`, `<=`, `&&`, and `||` before their single-character alternatives. Unknown characters raise an input exception.

### 2. Parsing — tokens to syntax

The recursive-descent parser builds typed AST nodes from the token stream. Its expression functions are organized by operator precedence: literals and variables, function calls, unary negation, powers, multiplication/division, addition/subtraction, comparisons, equality, logical AND, and logical OR. Each stage returns both the expression it parsed and the remaining tokens.

The statement parser handles declarations, assignments, printing, conditional branches, `while` loops, inclusive-range `for` loops, and returns. A top-level parser assembles function definitions followed by `int main()`, while checking certain duplicate names and syntax errors.

### 3. Evaluation — syntax to behavior

The evaluator traverses the AST, carrying an environment of variable bindings and function definitions. It performs arithmetic and boolean operations, evaluates conditions, updates variables, executes loops, and invokes user-defined functions with their own parameter bindings.

It checks runtime value types and raises explicit exceptions for issues such as undeclared identifiers, invalid operations, wrong argument types, and division by zero. **The output is produced by executing the program, not just printing its syntax tree.**

## Example: function call

`examples/function-call.c`:

```c
int square(int x) {
  return x * x;
}

int main() {
  int result;
  result = square(5);
  printf(result);
  return result;
}
```

Expected output:

```text
25
```

More examples in `examples/` demonstrate a sum computed with `for` and a `while` loop followed by an `if/else` branch.

## Source layout

| File | Purpose |
| --- | --- |
| [`src/lexer.ml`](src/lexer.ml) | Character scanning and token generation |
| [`src/parser.ml`](src/parser.ml) | Recursive-descent parsing, grammar precedence, AST construction |
| [`src/eval.ml`](src/eval.ml) | Expression evaluation, statement execution, functions |
| [`src/tokenTypes.ml`](src/tokenTypes.ml) | Token variants |
| [`src/smallCTypes.ml`](src/smallCTypes.ml) | AST, values, and environment data types |
| [`src/utils.ml`](src/utils.ml) | I/O, formatting, and environment helpers |
| [`bin/interface.ml`](bin/interface.ml) | CLI: lexical analysis, parsing, and evaluation |
| [`examples/`](examples/) | Standalone SmallC example programs |
| [`scripts/smoke-test.sh`](scripts/smoke-test.sh) | Build and simple output checks |

## Build and run

Requires OCaml, Dune, and `ppx_deriving` for generated AST printers.

```bash
opam install dune ppx_deriving
dune build
```

From the project directory, the CLI exposes three modes:

```bash
# Print the token stream
dune exec bin/interface.exe -- lex examples/function-call.c

# Print the parsed AST
dune exec bin/interface.exe -- parse examples/function-call.c

# Execute the SmallC program
dune exec bin/interface.exe -- eval examples/function-call.c

# Execute and display final variable bindings
dune exec bin/interface.exe -- eval examples/function-call.c --report
```

To build and compare the example outputs on Linux/macOS or WSL:

```bash
bash scripts/smoke-test.sh
```

**Verification note:** The source has undergone static review, but the OCaml build and smoke checks still need to run on a machine with the OCaml toolchain installed.

## Scope and limitations

SmallC is intentionally a small teaching language, **not a complete C compiler**. Its `for (i from start to end)` syntax differs from standard C; variable names must start with letters; and functions are expected to finish with a return. The tokenizer's handling of adjacent subtraction and signed integer literals also has limitations, so `a - 1` is preferable to `a-1`.

## Why I like this project

The most interesting challenge is making the individual stages work together. Lexing is about scanning characters accurately, parsing is about enforcing grammar and precedence, and evaluation is about keeping program state consistent across nested expressions, branches, loops, and functions. It's a hands-on introduction to the core ideas behind interpreters, compilers, query languages, and developer tooling.
