(* This file implements a fragment, that contains
  both the natural numbers, booleans, and also a an
  "cross operation" between them, the `iszero` function *)

open Fragment

(* BnIsZero fragment with IsZero

  For now this is a closed language fragment, i.e. it
  cannot be further extended. This limitation can be fixed
  fairly easily though.

  We need the UntiedCombine functor to (1) be able to reuse
  the combined eval, parse and pp methods that can already be
  constructed using the combiner, (2) but still leave the
  language open for further features.
*)
module BnIsZero = struct
  module BN =
    UntiedCombine (Nat_fragment.NatFragment) (Bool_fragment.BoolFragment)

  type term = BN of term BN.node | IsZero of term

  let inject_bn n = BN n
  let project_bn = function BN v -> Some v | _ -> None

  let fmap ~f = function
    | BN n -> inject_bn (BN.fmap ~f n)
    | IsZero n -> IsZero (f n)

  let fmap_aux = fun f -> fmap ~f

  let rec eval t =
    let rec is_number = function
      | BN t -> (
          match t with
          | BN.R _ -> false
          | BN.L nat_t -> (
              match nat_t with
              | Nat_fragment.NatFragment.Zero -> true
              | Nat_fragment.NatFragment.Succ wrapped_t
              | Nat_fragment.NatFragment.Pred wrapped_t ->
                  is_number wrapped_t))
      | IsZero _ -> false
    in
    match t with
    | IsZero it ->
        let eval_it = eval it in
        if is_number eval_it then
          match eval_it with
          | BN (BN.L Nat_fragment.NatFragment.Zero) ->
              BN (BN.R Bool_fragment.BoolFragment.True)
          | _ -> BN (BN.R Bool_fragment.BoolFragment.False)
        else t
    | BN it ->
        BN.eval ~inject:inject_bn ~project:project_bn ~full_eval:eval
          ~full_map:fmap_aux it

  let rec parse p =
    Input.skip_ws p;
    match BN.parse ~inject:inject_bn ~p ~full_parser:parse with
    | Some _ as v -> v
    | None -> (
        match Input.peek_token p with
        | "iszero" -> (
            ignore (Input.consume_token p);
            match parse p with Some t -> Some (IsZero t) | _ -> None)
        | _ -> None)

  let rec pp = function
    | IsZero t -> Printf.sprintf "iszero(%s)" (pp t)
    | BN t -> BN.pp ~full_pp:pp t
end
