# tapl

An extensible, composable language framework in OCaml, inspired by [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) and [Data Types à la Carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409).

Languages are built by composing independent fragments, each contributing term constructors, an evaluator, a parser, and a pretty-printer. The `Combine` functor wires them together into a closed language at the fixed point of their coproduct functor.

Each fragment is fully independent — no fragment's implementation knows about any other. This makes it straightforward to build languages with different combinations of features without rewriting large match expressions or tangled evaluation logic.

### Type checking

Typed fragments extend this with a type system: each fragment also contributes a `get_type` function, and `TypedUntiedCombine` / `TypedTie` wire them into a closed type checker in the same open-recursive style.


#### Type annotations

Abstractions are typed by annotating the whole term with an arrow type after a `:`. The annotation goes outside the parentheses that group the term:

```
( abs var 0 ) : arr Bool Bool       (* λ(x:Bool). x  :  Bool -> Bool *)
( abs var 0 ) : arr Nat Nat         (* λ(x:Nat).  x  :  Nat  -> Nat  *)
```

Use `*` as a wildcard for the return type when you only need to specify the argument:

```
( abs var 0 ) : arr Bool *          (* Bool -> Bool, return type inferred *)
```

Nested abstractions annotate each layer separately:

```
( abs ( abs var 1 ) : arr Nat * ) : arr Bool *
(* λ(x:Bool). λ(y:Nat). x  :  Bool -> Nat -> Bool *)
```

Arrow types are written in prefix form: `arr <arg-type> <return-type>`. Compound argument types need no extra parens since `arr` always takes exactly two arguments:

```
( abs app var 0 true ) : arr arr Bool Bool *
(* λ(f:Bool→Bool). f true  :  (Bool -> Bool) -> Bool *)
```

Types are printed in standard arrow notation with left-associative parentheses:

```
Bool -> Bool
Nat  -> Nat
(Bool -> Bool) -> Bool
(Bool -> Bool) -> Bool -> Bool
```

## Usage

### Build

```sh
opam install alcotest   # first time only
dune build
```

### Run

```sh
dune exec ./main.exe -- (-fragments <f1,f2,...> | -language <name>) (-code <expr> | -f <file>)
```

Exactly one source (`-fragments` or `-language`) and one input (`-code` or `-f`) must be given.

Terms are written in [Polish notation](https://en.wikipedia.org/wiki/Polish_notation) — each operator precedes its arguments, with fixed arity. This keeps parsing simple and unambiguous, since operator keywords are prefix-free. Subterms can be grouped with parentheses.

```sh
# Boolean conditionals
dune exec ./main.exe -- -fragments bool -code "if true then false else true"

# Natural numbers
dune exec ./main.exe -- -fragments nat -code "pred succ succ zero"

# Lambda calculus with booleans (de Bruijn indices)
dune exec ./main.exe -- -fragments fn,bool -code "app abs if var 0 then false else true true"

# Pre-defined language
dune exec ./main.exe -- -language bniszero -code "iszero pred succ zero"

# Typed languages are not yet exposed through the CLI
```

### Test

```sh
dune test
```