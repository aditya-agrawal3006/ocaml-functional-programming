# SmallC Interpreter | OCaml

A miniature programming language interpreter built in **OCaml**, with a hand-written lexer, a recursive-descent parser, and an evaluator that executes an abstract syntax tree (AST).

Rather than recognizing a few isolated expressions, this project connects the major pieces of a programming language into one execution pipeline: it reads source code, understands its structure, checks values and types at runtime, tracks variable bindings, and executes control flow and function calls.

> **Background:** Developed through University of Maryland coursework. Language specifications, core type definitions, the command-line interface, and some helper utilities were supplied as starter material; the lexer, recursive-descent parser, and evaluator contain my implementation work.

## How it works

```text
SmallC program (.c)
         |
         v
  1. Lexer (lexer.ml)
     characters -> token stream
         |
         v
  2. Parser (parser.ml)
     tokens -> typed AST nodes
         |
         v
  3. Evaluator (eval.ml)
     AST + execution environment
         |
         v
  Program output + variable bindings
```

The **lexer** walks the source string and identifies integer and boolean literals, identifiers, keywords, delimiters, and operators. The **parser** applies an operator-precedence grammar and assembles tokens into expression, statement, and function-definition nodes. The **evaluator** recursively traverses those nodes and keeps an environment of declared variables and function definitions, reporting errors for unsupported operations or invalid usage.

### 1. Lexing: recognizing program structure

[`src/lexer.ml`](src/lexer.ml) is a hand-written tokenizer. It uses short character-scanning helpers to find numeric literals and identifiers, recognizes keywords such as `int`, `bool`, `if`, `while`, `for`, and `return`, and gives multi-character operators (`==`, `!=`, `<=`, `>=`, `&&`, `||`) priority over single-character tokens. Invalid characters raise an input exception rather than being ignored.

### 2. Parsing: tokens to an AST

[`src/parser.ml`](src/parser.ml) uses recursive descent to implement expression precedence, function calls, and nested control flow.

- Expression layers: literals/variables/calls, negation, exponentiation, multiplication/division, addition/subtraction, comparisons, equality, AND, then OR.
- Statements: declarations, typed assignments, printing, `if`/`else`, `while`, and inclusive-range `for` loops.
- Function definitions: typed parameters, a return expression, a required `int main()` entry point, and checks for duplicate function/parameter names.

Each parsing function returns **the unconsumed tokens together with its AST node**, making nested expressions and statements composable. The AST variants live in [`src/smallCTypes.ml`](src/smallCTypes.ml).

### 3. Evaluation: running the AST

[`src/eval.ml`](src/eval.ml) interprets expression and statement nodes while carrying an execution environment:

- **Expressions:** arithmetic, exponentiation, comparisons, boolean operations, variables, and calls to user-defined functions.
- **Type handling:** checks integer and boolean operands, assignment types, function argument types, and return types.
- **Execution:** sequential statements, branching, `while` loops, inclusive `for` loops, `printf(...)`, and `return`.
- **Functions:** evaluates arguments, binds parameters in a new function-local environment, and returns the resulting value.
- **Failures:** reports undeclared identifiers, type mismatches, invalid input, and division by zero through explicit exceptions.

The result is an actual interpreter: program state changes as each statement executes, rather than simply displaying parsed syntax.

## Example

The included [`examples/function-call.c`](examples/function-call.c) demonstrates a user-defined function, declaration, assignment, call, print, and return:

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

Expected terminal output:

```text
25
```

More examples cover a `for` loop and combined `while`/`if` execution in [`examples/`](examples/).

## Run it locally

Requires **OCaml**, **Dune** (compatible with the `dune-project` file), and the **ppx_deriving** package for AST printers. With opam:

```bash
opam install dune ppx_deriving
```

From the `smallc-interpreter` directory, build and run:

```bash
dune build

dune exec bin/interface.exe -- lex examples/function-call.c
dune exec bin/interface.exe -- parse examples/function-call.c
dune exec bin/interface.exe -- eval examples/function-call.c
```

- `lex` displays the tokens.
- `parse` displays the generated AST.
- `eval` executes the program and prints its output.
- `eval <file> --report` also displays the final environment.
- `eval <file> --unparse` displays a statement-tree representation instead of executing.

On Linux/macOS (or WSL), run the small included smoke-test suite:

```bash
bash scripts/smoke-test.sh
```

The script builds the project and compares output from three examples. **The script has not been executed in this packaging environment** because the OCaml toolchain is not installed here.

## Files

```text
smallc-interpreter/
├── README.md
├── dune-project
├── dune
├── .gitignore
├── src/
│   ├── lexer.ml / lexer.mli          # Text -> tokens
│   ├── parser.ml / parser.mli        # Tokens -> AST
│   ├── eval.ml / eval.mli            # AST -> execution
│   ├── tokenTypes.ml                 # Token definitions
│   ├── smallCTypes.ml                # AST + environment types
│   ├── utils.ml                      # Printing / parsing helpers
│   └── dune
├── bin/
│   ├── interface.ml                  # Command-line driver
│   └── dune
├── examples/                        # SmallC programs
└── scripts/
    └── smoke-test.sh
```

## Important limitations

SmallC is a deliberately small teaching language, **not a full C compiler**. It uses its own syntax and AST, including a `for (index from start to end)` loop, and intentionally does not support many standard C features. Its lexer accepts identifiers beginning with letters, not underscores, and subtraction next to positive integer literals can require whitespace (for example `a - 1`). Its supported grammar expects a return statement at the end of each function body. This project is intended to demonstrate language processing and interpreter design rather than C compatibility or production language tooling.

## Why this project matters

The interesting part is how the separate modules fit together. Lexing is a character-level problem; parsing introduces a formal grammar and precedence; evaluating requires recursive execution, variable environments, functions, and error handling. Connecting all three demonstrates the mechanics behind languages, compilers, expression engines, and other tools that turn human-readable text into structured behavior.
