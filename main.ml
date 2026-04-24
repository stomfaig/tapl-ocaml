module Lang =
  Fragment.Combine (Bool_fragment.BoolFragment) (Fn_fragment.FnFragment)

let () =
  let src =
    match Sys.argv with
    | [| _; expr |] -> Input.from_string expr
    | [| _; "-f"; path |] -> Input.from_file path
    | _ -> Input.from_string (read_line ())
  in
  match Lang.parse src with
  | Some t -> print_endline (Lang.pp (Lang.eval t))
  | None -> print_endline "parse error"
