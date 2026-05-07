let opt_str = Alcotest.(option string)

let eval_pp (module L : Fragment.LANGUAGE) input =
  match L.parse (Input.from_string input) with
  | Some t -> Some (L.pp (L.eval t))
  | None -> None

let check name frags input expected =
  Alcotest.test_case name `Quick (fun () ->
      let lang = RuntimeContext.build_language_from_frags frags in
      let result = eval_pp lang input in
      Alcotest.check opt_str name (Some expected) result)

let check_parse_fails name frags input =
  Alcotest.test_case name `Quick (fun () ->
      let lang = RuntimeContext.build_language_from_frags frags in
      let result = eval_pp lang input in
      Alcotest.check opt_str name None result)

let check_unsupported name frags =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name RuntimeContext.UnsupportedFragment (fun () ->
          ignore (RuntimeContext.build_language_from_frags frags)))

let () =
  Alcotest.run "builder"
    [
      ( "bool",
        [
          check "true" [ "bool" ] "true" "true";
          check "false" [ "bool" ] "false" "false";
          check "if true" [ "bool" ] "if true then false else true" "false";
          check "if false" [ "bool" ] "if false then false else true" "true";
        ] );
      ( "nat",
        [
          check "zero" [ "nat" ] "zero" "0";
          check "succ zero" [ "nat" ] "succ zero" "succ(0)";
          check "pred succ zero" [ "nat" ] "pred succ zero" "0";
        ] );
      ( "bool+nat",
        [
          check "true" [ "bool"; "nat" ] "true" "true";
          check "zero" [ "bool"; "nat" ] "zero" "0";
          check "if nat branches" [ "bool"; "nat" ]
            "if true then succ zero else zero" "succ(0)";
          check_parse_fails "nat term in bool-only language" [ "bool" ] "zero";
          check_parse_fails "bool term in nat-only language" [ "nat" ] "true";
        ] );
      ( "fn",
        [
          check "identity" [ "fn"; "bool" ] "app abs var 0 true" "true";
          check "constant" [ "fn"; "bool" ] "app abs true false" "true";
          check "not" [ "fn"; "bool" ]
            "app abs if var 0 then false else true true" "false";
          check "K combinator" [ "fn"; "bool" ]
            "app app abs abs var 1 true false" "true";
        ] );
      ( "bool+nat+fn",
        [
          check "apply succ" [ "fn"; "nat" ] "app abs succ var 0 zero" "succ(0)";
          check "if nat branches" [ "fn"; "bool"; "nat" ]
            "if true then succ zero else zero" "succ(0)";
          check "apply not to if" [ "fn"; "bool"; "nat" ]
            "app abs if var 0 then false else true if true then true else false"
            "false";
        ] );
      ( "errors",
        [
          check_unsupported "unknown fragment" [ "unknown" ];
          check_unsupported "one unknown in list" [ "bool"; "unknown" ];
        ] );
    ]
