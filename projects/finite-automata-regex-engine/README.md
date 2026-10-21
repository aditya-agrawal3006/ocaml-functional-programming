# Finite Automata & Regular Expression Engine

An OCaml implementation of finite-state machine algorithms, with utilities for recognizing strings, converting between automata, and representing their languages as regular expressions.

## Features

- **Regular expressions → NFAs:** Thompson-style construction for literals, epsilon, union, concatenation, and Kleene star.
- **NFA simulation:** transitions, epsilon closure, and string acceptance.
- **NFA → DFA:** subset construction over reachable state sets.
- **DFA → regular expressions:** state elimination with support for repeated transitions and loops.
- **Visualization:** a Graphviz exporter for NFA and DFA diagrams.

## Project structure

| File | Purpose |
| --- | --- |
| [`src/nfa.ml`](src/nfa.ml) | Transition algorithms, automata acceptance, determinization, and DFA-to-regex conversion |
| [`src/regexp.ml`](src/regexp.ml) | Regular expression to NFA construction |
| [`src/utils.ml`](src/utils.ml) | Automata data types, list-based set operations, and parsing helpers |
| [`bin/viz.ml`](bin/viz.ml) | Generates a PNG visualization from a regular expression |

## Build and run

Requires OCaml, Dune 3.13+, `re`, `ppx_deriving`, and Graphviz (`dot`). Install OCaml dependencies with:

```bash
opam install dune re ppx_deriving
```

From this project's directory:

```bash
dune build
dune exec bin/viz.exe
```

Enter `(a|b)*abb` to visualize the finite-state machine, then choose NFA or DFA. The visualizer creates `output.png` locally.

The parser supports lowercase literals, `E` (the empty string), `|`, `*`, and parentheses.

### Using the OCaml library

```ocaml
let automaton = Fsm.Regexp.nfa_of_string "(a|b)*abb" in
Fsm.Nfa.accept automaton "aabb"  (* true *)
```

**Representation detail:** This project's regular-expression type includes epsilon but does not have a distinct empty-language constructor. An automaton accepting no strings therefore cannot be converted into an exactly equivalent expression with this type.

## Background

Developed through **University of Maryland coursework**. The shared interfaces, parsing helpers, and some visualization scaffolding originate from the course. This version is organized around the implementation and omits grading and submission artifacts.
