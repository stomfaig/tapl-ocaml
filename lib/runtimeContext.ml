exception UnsupportedInput
exception UnsupportedFragment
exception UnsupportedLanguage

type t = { language : (module Fragment.LANGUAGE); input : Input.t }

open Config

let frag_map = function
  | "nat" -> (module Nat_fragment.NatFragment : Fragment.FRAGMENT)
  | "bool" -> (module Bool_fragment.BoolFragment : Fragment.FRAGMENT)
  | "fn" -> (module Fn_fragment.FnFragment : Fragment.FRAGMENT)
  | _ -> raise UnsupportedFragment

let build_language_from_frags s =
  let init = (module Fragment.EmptyFragment : Fragment.FRAGMENT) in
  let combined_frags =
    List.fold_left
      (fun acc s ->
        let (module Acc : Fragment.FRAGMENT) = acc in
        let (module Frag : Fragment.FRAGMENT) = frag_map s in
        (module Fragment.UntiedCombine (Acc) (Frag) : Fragment.FRAGMENT))
      init s
  in
  let (module F : Fragment.FRAGMENT) = combined_frags in
  (module Fragment.Tie (F) : Fragment.LANGUAGE)

let lang_map = function
  | "bniszero" -> (module Bn_iszero.BnIsZero : Fragment.LANGUAGE)
  | _ -> raise UnsupportedLanguage

let build_context_from_config vc =
  let language =
    match vc.source with
    | Fragments frags -> build_language_from_frags frags
    | Language lang -> lang_map lang
  in
  let input =
    match vc.input with
    | File _file -> raise UnsupportedInput
    | Code code -> Input.from_string code
  in
  { language; input }
