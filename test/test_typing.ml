module L =
  TypedFragment.TypedCombine
    (Nat_fragment.TypedNatFragment)
    (Bool_fragment.TypedBoolFragment)

let parse s =
  match L.parse (Input.from_string s) with
  | ParseResult.Ok t -> t
  | ParseResult.Failed { msg; _ } -> Alcotest.failf "parse error: %s (%s)" s msg
  | ParseResult.Skip -> Alcotest.failf "parse error: %s (unrecognised)" s

let ty = Alcotest.(option string)
let infer s = Option.map L.pp_ty (L.get_type (parse s))

let well_typed name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name (Some expected) (infer input))

let ill_typed name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name None (infer input))

(* STLC: Nat + Bool + typed lambda calculus.
   Type annotations on abstractions are value witnesses: write a term of the
   desired type, e.g. [true] for Bool and [zero] for Nat.
   [abs true body] means "λ(x:Bool). body". *)
module LF =
  TypedFragment.TypedTie
    (TypedFragment.TypedUntiedCombine
       (TypedFragment.TypedUntiedCombine
          (Nat_fragment.TypedNatFragment)
          (Bool_fragment.TypedBoolFragment))
          (Fn_fragment.TyFnFragment))

let parse_lf s =
  match LF.parse (Input.from_string s) with
  | ParseResult.Ok t -> t
  | ParseResult.Failed { msg; _ } -> Alcotest.failf "parse error: %s (%s)" s msg
  | ParseResult.Skip -> Alcotest.failf "parse error: %s (unrecognised)" s

let infer_lf s = Option.map LF.pp_ty (LF.get_type (parse_lf s))

let wt name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name (Some expected) (infer_lf input))

let it name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name None (infer_lf input))

let () =
  Alcotest.run "typing"
    [
      ( "well-typed",
        [
          well_typed "true" "true" "Bool";
          well_typed "false" "false" "Bool";
          well_typed "zero" "zero" "Nat";
          well_typed "succ zero" "succ zero" "Nat";
          well_typed "pred zero" "pred zero" "Nat";
          well_typed "if bool" "if true then false else true" "Bool";
          well_typed "if nat" "if true then succ zero else zero" "Nat";
        ] );
      ( "ill-typed",
        [
          ill_typed "non-bool condition" "if zero then true else false";
          ill_typed "branch type mismatch" "if true then zero else true";
        ] );
      ( "stlc well-typed",
        [
          wt "bool identity" "abs true var 0" "Bool -> Bool";
          wt "nat identity" "abs zero var 0" "Nat -> Nat";
          wt "bool const" "abs true abs zero var 1" "Bool -> Nat -> Bool";
          wt "apply identity to bool" "app abs true var 0 false" "Bool";
          wt "apply identity to nat" "app abs zero var 0 succ zero" "Nat";
          wt "higher-order arg" "abs abs true var 0 app var 0 true"
            "(Bool -> Bool) -> Bool";
          wt "church-style compose"
            "abs abs true var 0 abs true app var 1 app var 1 var 0"
            "(Bool -> Bool) -> Bool -> Bool";
        ] );
      ( "stlc ill-typed",
        [
          it "var in empty ctx" "var 0";
          it "apply non-function" "app true false";
          it "argument type mismatch" "app abs true var 0 zero";
          it "nat applied as bool" "app abs zero var 0 true";
        ] );
    ]
