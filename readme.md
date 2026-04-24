# tapl

An extensible, composable language framework in OCaml, inspired by [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) and [Data Types à la Carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409).

Languages are built by composing independent fragments, each contributing a set of term constructors, an evaluator, a parser, and a pretty-printer. The `Combine` functor wires them together into a closed language at the fixed point of their coproduct functor. 

This promotes the fact that different language fragments can be highly independent, and that their implementations should not depend on each other. This also allows greater flexibility in building languages with properties more easily, than reimplementing a complete closed system every time with huge match cases, and complex evaluation logic.

For now only simple fragments are to be supported (e.g. lambda calculus, nat, bool), soon with typed versions too (see `TYPED_FRAGMENT` and the in progress `context`)

## How tos

### Build

```sh
opam install alcotest   # first time only
dune build
```

### Run

```sh
dune exec ./main.exe                           # read one line from stdin
dune exec ./main.exe -- "app abs var 0 true"   # inline expression
dune exec ./main.exe -- -f prog.lam            # from file
```

Example:

```sh
$ dune exec ./main.exe -- "app abs if var 0 then true else false true"
true
```

### Test

```sh
dune test
```