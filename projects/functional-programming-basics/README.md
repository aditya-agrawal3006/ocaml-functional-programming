# Functional Programming Fundamentals (OCaml)

A collection of OCaml functions practicing functional programming from first principles.

## Concepts demonstrated
- Immutable data and pattern matching with tuples and lists.
- Recursion for Fibonacci numbers, powers, logarithms, and greatest common factors.
- List operations including reversal, zipping, merging, membership checks, and periodic selections.
- Higher-order programming with `map`, `fold`, and `fold_right`.
- Composing functions to calculate list and sequence results.

## Main files
- [`src/basics.ml`](src/basics.ml) — implementations of the exercises.
- [`src/basics.mli`](src/basics.mli) — types and function signatures.
- [`src/funs.ml`](src/funs.ml) — supporting higher-order functions.

## Build
Requires OCaml, Dune, and an available OCaml compiler:

```bash
dune build
```

This project is a library of functions, not a standalone executable. Refer to the `.mli` files for callable functions.

> Based on University of Maryland coursework. Some interfaces and scaffolding were supplied by the course.
