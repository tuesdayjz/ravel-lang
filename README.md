# Ravel

`Ravel` is a tiny prototype functional language that compiles to interaction nets.

## Syntax

```text
program    ::= definition* expr

definition ::= def NAME(pattern[, pattern ...]) = expr

pattern    ::= 0
             | succ(pattern)
             | CONSTRUCTOR
             | CONSTRUCTOR(pattern, ...)
             | NAME
             | _

expr       ::= INT
             | NAME
             | CONSTRUCTOR
             | succ(expr)
             | NAME(expr, ...)
             | CONSTRUCTOR(expr, ...)
             | let NAME = expr in expr
             | dup expr as NAME, NAME in expr
             | drop expr in expr
             | (expr)
```

### Notes

- `add(x, y)` is the built-in addition function.
- User-defined functions may be recursive, including mutually recursive.
- Function clauses can pattern-match on any argument.
- Constructor patterns can be nested to any depth, such as
  `Cons(Cons(x, _), tail)`.
- Patterns support `0`, recursive `succ(...)`, user constructors, variable
  binders, and the `_` wildcard.
- Clauses are considered in source order; variable and wildcard patterns can
  be used as fallbacks after more specific clauses.
- Natural-number matches must cover both `0` and `succ(...)`, unless a
  variable or `_` fallback covers the remaining values.
- Concurrency is currently **implicit**: independent subexpressions become
  independent subnets, so they can reduce in different interleavings even
  though the language does not yet expose channels, spawning, logic
  variables, or backtracking.

## Linearity

Variables are **linear**, including function parameters and binders at any
depth in a pattern:

- use a variable at most once
- if you need it twice, duplicate explicitly with `dup`
- if you do not need a value, erase it explicitly with `drop`

Example:

```text
def plus(0, y) = y

def plus(succ(x), y) = succ(plus(x, y))

def double(n) = dup n as a, b in plus(a, b)

double(3)
```

Deep and multi-argument patterns compose directly:

```text
def lookup(0, Cons(value, _)) = value

def lookup(0, Nil) = 0

def lookup(succ(index), Cons(_, rest)) = lookup(index, rest)

def lookup(succ(_), Nil) = 0

lookup(2, Cons(4, Cons(5, Cons(6, Nil))))
```

## Build and Install

Build and install for your user into `~/.local/bin`:

```sh
make && make install
```

If `~/.local/bin` is not already on your `PATH`, add this to your shell config
(`~/.bashrc`, `~/.zshrc`, etc.), then reload your shell:

```sh
export PATH="$HOME/.local/bin:$PATH"
```

After that you can run:

```sh
ravel examples/plus.rvl
```

For a system-wide install instead:

```sh
sudo make install PREFIX=/usr/local
```

To uninstall:

```sh
make uninstall
# or
sudo make uninstall PREFIX=/usr/local
```

## Test

```sh
make test
```

## Run

Evaluate a file:

```sh
./ravel examples/plus.rvl
```

Evaluate inline source:

```sh
./ravel -e "def id(n) = n
id(4)"
```

Trace reduction:

```sh
./ravel --trace examples/double.rvl
```

Try a different reduction strategy:

```sh
./ravel --trace --strategy random --seed 42 examples/double.rvl
```

## Example programs

See `examples/`

### Functional

- `examples/fact.rvl` — factorial via linear multiplication
- `examples/deep_patterns.rvl` — nested constructor matching with an ordered
  fallback clause
- `examples/multi_argument_patterns.rvl` — list lookup by matching both the
  index and list arguments

### Logic-flavored

- `examples/logic_arith.rvl` — arithmetic predicates (`eq`, `leq`, `not`,
  `and`) returning `0/1`

### Concurrency-flavored

- `examples/work_pool.rvl` — several independent worker computations composed
  into one result

Try comparing interleavings on the work-pool example:

```sh
./ravel --trace --strategy first examples/work_pool.rvl
./ravel --trace --strategy random --seed 42 examples/work_pool.rvl
```

Both runs should compute the same final value, while the reduction order may differ.
