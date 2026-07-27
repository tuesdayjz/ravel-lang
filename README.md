# Ravel

Ravel is a small functional language that compiles to interaction nets.

## Features

- User-defined constructor families with `data`
- Deep, multi-argument pattern matching
- First-class named functions, lambdas, and lexical closures
- Recursive and mutually recursive functions
- Automatic labeled `Dup` and `Era` insertion
- Explicit `dup` and `drop` for low-level control
- Deterministic or random interaction-net reduction

## Syntax

```text
program    ::= typedef* definition* expr
typedef    ::= data TYPE_NAME = CONSTRUCTOR ('|' CONSTRUCTOR)*
definition ::= def NAME(pattern[, pattern ...]) = expr

pattern    ::= 0
             | succ(pattern)
             | CONSTRUCTOR(pattern, ...)
             | CONSTRUCTOR
             | NAME
             | _

expr       ::= INT
             | NAME
             | CONSTRUCTOR
             | succ(expr)
             | expr(expr, ...)
             | CONSTRUCTOR(expr, ...)
             | fun(NAME[, NAME ...]) = expr
             | let NAME = expr in expr
             | dup expr as NAME, NAME in expr
             | drop expr in expr
             | (expr)
```

A `data` declaration groups constructor names. Constructor arities are inferred
from usage. Declared families are checked for exhaustive matching; built-in
`Nat = Z | S` uses the same mechanism.

## Example

```ravel
data Option = None | Some

def map_option(f, None) = None

def map_option(f, Some(value)) = Some(f(value))

def twice(f, value) = f(f(value))

def make_adder(offset) = fun(value) = add(offset, value)

let add_two = make_adder(2) in
twice(add_two, 3)
```

Variables do not require manual resource management. The compiler inserts:

- `Era` for zero uses
- a direct wire for one use
- a fresh labeled `Dup` tree for multiple uses

Explicit `dup` and `drop` remain supported.

## Build and test

Install the opam-managed dependencies, then build with Dune:

```sh
opam install . --deps-only --with-test
make
make test
```

Install for the current user:

```sh
make install
```

## Run

```sh
ravel examples/closures.rvl
ravel -e "let f = fun(x) = succ(x) in f(4)"
```

Useful options:

```sh
ravel --trace examples/closures.rvl
ravel --dump-ast examples/closures.rvl
ravel --dump-net examples/closures.rvl
ravel --strategy random --seed 42 examples/closures.rvl
```

## Examples

- `examples/closures.rvl` — closures and higher-order functions
- `examples/adt_tree_lookup.rvl` — recursive ADTs and optional results
- `examples/deep_patterns.rvl` — nested constructor matching
- `examples/multi_argument_patterns.rvl` — matching across arguments
- `examples/fact.rvl` — recursive arithmetic
- `examples/logic_arith.rvl` — arithmetic predicates
- `examples/work_pool.rvl` — independent concurrent reductions
