module BN =
  Fragment.Combine (Bool_fragment.BoolFragment) (Nat_fragment.NatFragment)

module LB =
  Fragment.Combine (Bool_fragment.BoolFragment) (Fn_fragment.FnFragment)

let parse_bn s = BN.parse (Input.from_string s)
let parse_lb s = LB.parse (Input.from_string s)
let opt_str = Alcotest.(option string)

(* Round-trip: parse then pp *)
let rt_bn name input expected_pp =
  Alcotest.test_case name `Quick (fun () ->
      let result = Option.map BN.pp (parse_bn input) in
      Alcotest.check opt_str name (Some expected_pp) result)

let rt_lb name input expected_pp =
  Alcotest.test_case name `Quick (fun () ->
      let result = Option.map LB.pp (parse_lb input) in
      Alcotest.check opt_str name (Some expected_pp) result)

let fails_bn name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check opt_str name None (Option.map BN.pp (parse_bn input)))

let fails_lb name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check opt_str name None (Option.map LB.pp (parse_lb input)))

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
      ( "errors",
        [
          fails_bn "empty" "";
          fails_bn "unknown token" "blah";
          fails_bn "incomplete succ" "succ";
          fails_bn "incomplete if" "if true";
          fails_lb "incomplete app" "app abs var 0";
          fails_lb "var no index" "var";
        ] );
    ]
