open ParseResult

module BN =
  Fragment.Combine (Bool_fragment.BoolFragment) (Nat_fragment.NatFragment)

module LB =
  Fragment.Combine (Bool_fragment.BoolFragment) (Fn_fragment.FnFragment)

module BNIsZero = Bn_iszero.BnIsZero

let parse_bn s = BN.parse (Input.from_string s)
let parse_lb s = LB.parse (Input.from_string s)
let parse_bniz s = BNIsZero.parse (Input.from_string s)

let rt name parse pp input expected_pp =
  Alcotest.test_case name `Quick (fun () ->
      match parse input with
      | ParseResult.Ok t -> Alcotest.check Alcotest.string name expected_pp (pp t)
      | ParseResult.Failed { msg; _ } -> Alcotest.failf "unexpected Failed: %s" msg
      | ParseResult.Skip -> Alcotest.fail "unexpected Skip")

let rt_bn name = rt name parse_bn BN.pp
let rt_lb name = rt name parse_lb LB.pp
let rt_bniz name = rt name parse_bniz BNIsZero.pp

let parse_skips name parse input =
  Alcotest.test_case name `Quick (fun () ->
      match parse input with
      | ParseResult.Skip -> ()
      | ParseResult.Ok _ -> Alcotest.fail "expected Skip but parsed successfully"
      | ParseResult.Failed { msg; _ } ->
          Alcotest.failf "expected Skip but got Failed: %s" msg)

let parse_fails name parse input =
  Alcotest.test_case name `Quick (fun () ->
      match parse input with
      | ParseResult.Failed _ -> ()
      | ParseResult.Ok _ -> Alcotest.fail "expected Failed but parsed successfully"
      | ParseResult.Skip -> Alcotest.fail "expected Failed but got Skip")

let () =
  Alcotest.run "parse"
    [
      ( "bool",
        [
          rt_bn "true" "true" "true";
          rt_bn "false" "false" "false";
          rt_bn "if" "if true then false else true"
            "if true then false else true";
          rt_bn "nested if"
            "if true then if false then true else false else true"
            "if true then if false then true else false else true";
        ] );
      ( "nat",
        [
          rt_bn "zero" "zero" "0";
          rt_bn "succ zero" "succ zero" "succ(0)";
          rt_bn "pred zero" "pred zero" "pred(0)";
          rt_bn "succ succ" "succ succ zero" "succ(succ(0))";
          rt_bn "pred succ" "pred succ zero" "pred(succ(0))";
          rt_bn "if nat" "if true then succ zero else zero"
            "if true then succ(0) else 0";
        ] );
      ( "lambda",
        [
          rt_lb "var" "var 0" "0";
          rt_lb "abs" "abs var 0" "\xce\xbb. 0";
          rt_lb "app" "app abs var 0 true" "(\xce\xbb. 0 true)";
          rt_lb "K" "app app abs abs var 1 true false"
            "((\xce\xbb. \xce\xbb. 1 true) false)";
          rt_lb "abs if" "abs if var 0 then true else false"
            "\xce\xbb. if 0 then true else false";
        ] );
      ( "bn iszero",
        [
          rt_bniz "true" "true" "true";
          rt_bniz "false" "false" "false";
          rt_bniz "if" "if true then false else true"
            "if true then false else true";
          rt_bniz "nested if"
            "if true then if false then true else false else true"
            "if true then if false then true else false else true";
          rt_bniz "zero" "zero" "0";
          rt_bniz "succ zero" "succ zero" "succ(0)";
          rt_bniz "pred zero" "pred zero" "pred(0)";
          rt_bniz "succ succ" "succ succ zero" "succ(succ(0))";
          rt_bniz "pred succ" "pred succ zero" "pred(succ(0))";
          rt_bniz "if nat" "if true then succ zero else zero"
            "if true then succ(0) else 0";
          rt_bniz "iszero" "iszero zero" "iszero(0)";
          rt_bniz "iszero2" "iszero succ zero" "iszero(succ(0))";
          rt_bniz "iszero3" "iszero succ if true then zero else succ zero"
            "iszero(succ(if true then 0 else succ(0)))";
        ] );
      ( "errors",
        [
          parse_skips "empty" parse_bn "";
          parse_skips "unknown token" parse_bn "blah";
          parse_skips "incomplete succ" parse_bn "succ";
          parse_fails "incomplete if" parse_bn "if true";
          parse_skips "incomplete app" parse_lb "app abs var 0";
          parse_fails "var no index" parse_lb "var";
        ] );
    ]
