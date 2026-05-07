exception InvalidConfig

type t = {
  mutable fragment : string list option ref;
  mutable language : string option ref;
  mutable filename : string option ref;
  mutable code : string option ref;
}

let make () =
  {
    fragment = ref None;
    language = ref None;
    filename = ref None;
    code = ref None;
  }

type input = File of string | Code of string
type source = Fragments of string list | Language of string
type validated_config = { source : source; input : input }

let validate_config c =
  let source =
    match (!(c.fragment), !(c.language)) with
    | None, Some lang -> Language lang
    | Some frags, None -> Fragments frags
    | _ -> raise InvalidConfig
  in
  let input =
    match (!(c.filename), !(c.code)) with
    | None, Some code -> Code code
    | Some filename, None -> File filename
    | _ -> raise InvalidConfig
  in
  { source; input }
