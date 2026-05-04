module BN =
  Fragment.Combine (Bool_fragment.BoolFragment) (Nat_fragment.NatFragment)

module LB =
  Fragment.Combine (Bool_fragment.BoolFragment) (Fn_fragment.FnFragment)

module BNIsZero = Bn_iszero.BnIsZero

let term (type a) pp =
  Alcotest.testable (fun ppf t -> Format.pp_print_string ppf (pp t)) ( = )

(* Bool + Nat language helpers *)

let bn_term = term BN.pp

let bn s =
  match BN.parse (Input.from_string s) with
  | Some t -> t
  | None -> Alcotest.failf "parse error: %s" s

let tt = BN.L Bool_fragment.BoolFragment.True
let ff = BN.L Bool_fragment.BoolFragment.False
let zero = BN.R Nat_fragment.NatFragment.Zero
let succ n = BN.R (Nat_fragment.NatFragment.Succ n)

let check_bn name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check bn_term name expected (BN.eval (bn input)))

(* Lambda + Bool language helpers *)

let lb_term = term LB.pp

let lb s =
  match LB.parse (Input.from_string s) with
  | Some t -> t
  | None -> Alcotest.failf "parse error: %s" s

let ltt = LB.L Bool_fragment.BoolFragment.True
let lff = LB.L Bool_fragment.BoolFragment.False

let check_lb name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check lb_term name expected (LB.eval (lb input)))

let bniz_term = term BNIsZero.pp

let bniz s =
  match BNIsZero.parse (Input.from_string s) with
  | Some t -> t
  | None -> Alcotest.failf "parse error: %s" s

let bnizz = BNIsZero.BN.L Nat_fragment.NatFragment.Zero
let bnizs n = BNIsZero.BN.L (Nat_fragment.NatFragment.Succ n)
let bnizt = BNIsZero.BN.R Bool_fragment.BoolFragment.True
let bnizf = BNIsZero.BN.R Bool_fragment.BoolFragment.False
let bniziz n = BNIsZero.IsZero n

let check_bniz name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check bniz_term name expected (BNIsZero.eval (bniz input)))

(* Tie + UntiedCombine: Bool at the outer level, so FnFragment must cross the
   fragment boundary when parsing sub-terms like `if`. *)
module TieFnNatBool =
  Fragment.Tie
    (Fragment.UntiedCombine
       (Fragment.UntiedCombine
          (Fn_fragment.FnFragment)
          (Nat_fragment.NatFragment))
          (Bool_fragment.BoolFragment))

let tie s =
  match TieFnNatBool.parse (Input.from_string s) with
  | Some t -> TieFnNatBool.pp (TieFnNatBool.eval t)
  | None -> Alcotest.failf "parse error: %s" s

let check_tie name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected (tie input))

let () =
  Alcotest.run "eval"
    [
      ( "bool",
        [
          check_bn "true is a value" "true" tt;
          check_bn "false is a value" "false" ff;
          check_bn "if true" "if true then false else true" ff;
          check_bn "if false" "if false then false else true" tt;
          check_bn "nested condition"
            "if if true then false else true then true else false" ff;
          check_bn "nested branches"
            "if true then if false then true else false else true" ff;
        ] );
      ( "nat",
        [
          check_bn "zero is a value" "zero" zero;
          check_bn "succ zero" "succ zero" (succ zero);
          check_bn "pred zero" "pred zero" zero;
          check_bn "pred succ zero" "pred succ zero" zero;
          check_bn "pred succ succ" "pred succ succ zero" (succ zero);
          check_bn "succ pred succ" "succ pred succ zero" (succ zero);
          check_bn "if nat branches" "if true then succ zero else zero"
            (succ zero);
        ] );
      ( "bniz",
        [
          check_bniz "zero is a value" "zero" (bniz "zero");
          check_bniz "true is a value" "true" (bniz "true");
          check_bniz "false is a value" "false" (bniz "false");
          check_bniz "iszero zero" "iszero zero" (bniz "true");
          check_bniz "iszero succ zero" "iszero succ zero" (bniz "false");
          check_bniz "iszero succ succ zero" "iszero succ succ zero"
            (bniz "false");
          check_bniz "iszero pred succ zero" "iszero pred succ zero"
            (bniz "true");
        ] );
      ( "lambda",
        [
          check_lb "identity" "app abs var 0 true" ltt;
          check_lb "constant" "app abs true false" ltt;
          check_lb "not" "app abs if var 0 then false else true true" lff;
          check_lb "subst through if"
            "app abs if var 0 then true else false true" ltt;
          check_lb "K combinator" "app app abs abs var 1 true false" ltt;
          check_lb "nested application" "app abs var 0 app abs var 0 false" lff;
          check_lb "identity of identity" "app app abs abs var 0 abs var 0 true"
            ltt;
          Alcotest.test_case "abstraction is a value" `Quick (fun () ->
              let t = lb "abs var 0" in
              Alcotest.check lb_term "abs var 0" t (LB.eval t));
        ] );
      ( "tie",
        [
          check_tie "identity" "app abs var 0 true" "true";
          check_tie "constant" "app abs true false" "true";
          check_tie "nat body" "app abs succ var 0 zero" "succ(0)";
          check_tie "not - bool body across fragment boundary"
            "app abs if var 0 then false else true true" "false";
          check_tie "subst through if across boundary"
            "app abs if var 0 then true else false true" "true";
          check_tie "K combinator" "app app abs abs var 1 true false" "true";
          check_tie "apply not to if"
            "app abs if var 0 then false else true if true then true else false"
            "false";
          check_tie "if selects fn branch"
            "if true then app abs var 0 zero else succ zero" "0";
        ] );
    ]
