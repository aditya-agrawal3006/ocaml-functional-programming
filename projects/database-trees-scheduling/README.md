# Functional Database, Trees & Scheduling (OCaml)

Three functional-programming components written in OCaml: a small in-memory person database, tree-processing functions, and scheduling algorithms.

## Components

**In-memory database (`src/db.ml`):** Store person records, insert/remove entries, compose filtering conditions (`And`, `Or`, `Not`, `If`), sort query results, and update matching records without mutating the original list.

**Tree algorithms (`src/tree.ml`):** Define binary-tree folds and build mapping, in-order/pre-order traversals, function composition, depth calculation, and tree trimming on top of a reusable fold.

**Scheduling (`src/scheduling.ml`):** Check whether bookings overlap in the same room and compare a schedule's size against a greedy interval-scheduling count of compatible requests.

## Project structure
- `src/*.ml` — implementations.
- `src/*.mli` — module signatures.
- `dune-project` and `src/dune` — OCaml build configuration.

## Build
Requires OCaml, Dune, and the PPX libraries referenced in `src/dune` (`ppx_jane`, `ppx_deriving.show`, `ppx_inline_test`, and `ppx_assert`).

```bash
dune build
```

The project exposes library functions rather than a standalone program. Build success and algorithm correctness have not been independently verified in this curated package.

> Based on University of Maryland coursework, with course-provided interfaces and specifications.
