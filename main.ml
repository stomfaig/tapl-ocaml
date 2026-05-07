module Lang =
  Fragment.Combine (Bool_fragment.BoolFragment) (Fn_fragment.FnFragment)

let usage_message =
  "Usage: main (-fragments <f1,f2,...> | -language <name>) (-f <file> | -code \
   <expr>)\n\
   Builds a TAPL language from fragments or a pre-defined language and \
   evaluates the given source."

let config = Config.make ()

let parse_fragments s =
  let fragments = String.split_on_char ',' s in
  config.fragment := Some fragments

let speclist =
  [
    ( "-fragments",
      Arg.String parse_fragments,
      "Comma separated list of fragments to construct the language from" );
    ( "-language",
      Arg.String (fun s -> config.language := Some s),
      "Pre-defined language to use (instead of building one from fragments)" );
    ( "-f",
      Arg.String (fun s -> config.filename := Some s),
      "(Optional) File path for file to execute." );
    ( "-code",
      Arg.String (fun s -> config.code := Some s),
      "(Optional) Inline code to execute." );
  ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_message;
  let vc = Config.validate_config config in
  let conf = RuntimeContext.build_context_from_config vc in
  let (module Lang : Fragment.LANGUAGE) = conf.language in
  let parsed = Lang.parse conf.input in
  match parsed with
  | Some result -> print_endline (Lang.pp result)
  | None -> print_endline "Parsing failed"
