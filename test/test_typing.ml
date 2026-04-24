module L =
  Fragment.TypedCombine
    (Nat_fragment.TypedNatFragment)
    (Bool_fragment.TypedBoolFragment)

let parse s =
  match L.parse (Input.from_string s) with
  | Some t -> t
  | None -> Alcotest.failf "parse error: %s" s

let ty = Alcotest.(option string)
let infer s = Option.map L.pp_ty (L.get_type (parse s))

let well_typed name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name (Some expected) (infer input))

let ill_typed name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check ty name None (infer input))

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
    ]
