# tapl

An extensible, composable language framework in OCaml, inspired by [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) and [Data Types à la Carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409).

Languages are built by composing independent fragments, each contributing a set of term constructors, an evaluator, a parser, and a pretty-printer. The `Combine` functor wires them together into a closed language at the fixed point of their coproduct functor. 

This promotes the fact that different language fragments can be highly independent, and that their implementations should not depend on each other. This also allows greater flexibility in building languages with properties more easily, than reimplementing a complete closed system every time with huge match cases, and complex evaluation logic.

Typed fragments extend this with a type system: each fragment also contributes a `get_type` function, and `TypedUntiedCombine` / `TypedTie` wire them into a closed type checker in the same open-recursive style.

## How tos

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

Note that the code passed to the interpreter must be follow [Polish notation](https://en.wikipedia.org/wiki/Polish_notation): each operator precedes its arguments, with arity fixed per operator. This makes parsing really easy. Since we assume that major-keywords are prefix-free, we can also parse the code eagerly. 

```sh
# Boolean conditionals
dune exec ./main.exe -- -fragments bool -code "if true then false else true"

# Natural numbers
dune exec ./main.exe -- -fragments nat -code "pred succ succ zero"

# Lambda calculus with booleans (de Bruijn indices, prefix notation)
dune exec ./main.exe -- -fragments fn,bool -code "app abs if var 0 then false else true true"

# Pre-defined language
dune exec ./main.exe -- -language bniszero -code "iszero pred succ zero"
```

### Type checking

Typed fragments can be assembled with `TypedUntiedCombine` and closed with `TypedTie`:

```ocaml
module L =
  TypedFragment.TypedTie (
    TypedFragment.TypedUntiedCombine (
      TypedFragment.TypedUntiedCombine (
        Nat_fragment.TypedNatFragment) (
        Bool_fragment.TypedBoolFragment)) (
      Fn_fragment.TyFnFragment))

let () =
  let t = ... (* parse a term *) in
  match L.get_type t with
  | Some ty -> Printf.printf "type: %s\n" (L.pp_ty ty)
  | None    -> Printf.printf "ill-typed\n"
```

`TyFnFragment` uses **type witnesses**: the annotation on an abstraction is a
term whose *type* names the argument type. Write `true` to mean Bool, `zero` to
mean Nat, and `abs true var 0` to mean `Bool -> Bool`.

```
abs true var 0           (* λ(x:Bool). x  :  Bool -> Bool *)
abs zero var 0           (* λ(x:Nat).  x  :  Nat  -> Nat  *)
abs abs true var 0       (* λ(f:Bool→Bool). …  *)
    app var 0 true       (*   f true  :  (Bool -> Bool) -> Bool *)
```

Types are printed in standard arrow notation with left-associative parentheses:

```
Bool -> Bool
Nat  -> Nat
(Bool -> Bool) -> Bool
(Bool -> Bool) -> Bool -> Bool
```

### Test

```sh
dune test
```
