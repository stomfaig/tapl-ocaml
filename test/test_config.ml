let make ?fragment ?language ?filename ?code () =
  let c = Config.make () in
  (match fragment with Some f -> c.fragment := Some f | None -> ());
  (match language with Some l -> c.language := Some l | None -> ());
  (match filename with Some f -> c.filename := Some f | None -> ());
  (match code with Some s -> c.code := Some s | None -> ());
  c

let eval_pp (module L : Fragment.LANGUAGE) input =
  match L.parse (Input.from_string input) with
  | ParseResult.Ok t -> Some (L.pp (L.eval t))
  | _ -> None

(* validate_config *)

let check_valid name cfg check =
  Alcotest.test_case name `Quick (fun () ->
      match Config.validate_config cfg with
      | vc -> check vc
      | exception Config.InvalidConfig ->
          Alcotest.fail "expected valid config but got InvalidConfig")

let check_invalid name cfg =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name Config.InvalidConfig (fun () ->
          ignore (Config.validate_config cfg)))

(* build_context_from_config *)

let check_ctx name cfg input expected =
  Alcotest.test_case name `Quick (fun () ->
      let vc = Config.validate_config cfg in
      let ctx = RuntimeContext.build_context_from_config vc in
      let result = eval_pp ctx.language input in
      Alcotest.check Alcotest.(option string) name (Some expected) result)

let check_ctx_raises name cfg exn =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name exn (fun () ->
          let vc = Config.validate_config cfg in
          ignore (RuntimeContext.build_context_from_config vc)))

let () =
  Alcotest.run "config"
    [
      ( "validate - valid",
        [
          check_valid "fragments + code"
            (make ~fragment:[ "bool" ] ~code:"true" ()) (fun vc ->
              Alcotest.(check bool)
                "source is Fragments" true
                (match vc.source with Config.Fragments _ -> true | _ -> false));
          check_valid "language + code"
            (make ~language:"bniszero" ~code:"true" ()) (fun vc ->
              Alcotest.(check bool)
                "source is Language" true
                (match vc.source with Config.Language _ -> true | _ -> false));
          check_valid "fragments + file"
            (make ~fragment:[ "nat" ] ~filename:"x.tapl" ()) (fun vc ->
              Alcotest.(check bool)
                "input is File" true
                (match vc.input with Config.File _ -> true | _ -> false));
          check_valid "fragments + code input"
            (make ~fragment:[ "nat" ] ~code:"zero" ()) (fun vc ->
              Alcotest.(check bool)
                "input is Code" true
                (match vc.input with Config.Code _ -> true | _ -> false));
        ] );
      ( "validate - invalid",
        [
          check_invalid "no source" (make ~code:"true" ());
          check_invalid "both sources"
            (make ~fragment:[ "bool" ] ~language:"bniszero" ~code:"true" ());
          check_invalid "no input" (make ~fragment:[ "bool" ] ());
          check_invalid "both inputs"
            (make ~fragment:[ "bool" ] ~code:"true" ~filename:"x.tapl" ());
        ] );
      ( "build context - fragments",
        [
          check_ctx "bool true"
            (make ~fragment:[ "bool" ] ~code:"true" ())
            "true" "true";
          check_ctx "bool if"
            (make ~fragment:[ "bool" ] ~code:"if true then false else true" ())
            "if true then false else true" "false";
          check_ctx "nat pred succ"
            (make ~fragment:[ "nat" ] ~code:"pred succ zero" ())
            "pred succ zero" "0";
          check_ctx "fn identity"
            (make ~fragment:[ "fn"; "bool" ] ~code:"app abs var 0 true" ())
            "app abs var 0 true" "true";
          check_ctx "fn not cross-fragment"
            (make ~fragment:[ "fn"; "bool" ]
               ~code:"app abs if var 0 then false else true true" ())
            "app abs if var 0 then false else true true" "false";
        ] );
      ( "build context - named language",
        [
          check_ctx "bniszero true"
            (make ~language:"bniszero" ~code:"true" ())
            "true" "true";
          check_ctx "bniszero iszero zero"
            (make ~language:"bniszero" ~code:"iszero zero" ())
            "iszero zero" "true";
        ] );
      ( "build context - errors",
        [
          check_ctx_raises "unknown fragment"
            (make ~fragment:[ "unknown" ] ~code:"true" ())
            RuntimeContext.UnsupportedFragment;
          check_ctx_raises "unknown language"
            (make ~language:"unknown" ~code:"true" ())
            RuntimeContext.UnsupportedLanguage;
          check_ctx_raises "file input unsupported"
            (make ~fragment:[ "bool" ] ~filename:"x.tapl" ())
            RuntimeContext.UnsupportedInput;
        ] );
    ]
