open Fragment

module NatFragment = struct
  type 'a node = Zero | Succ of 'a | Pred of 'a

  let eval ~inject ~project ~full_eval ~full_map:_ = function
    | Zero as v -> inject v
    | Succ t -> inject (Succ (full_eval t))
    | Pred t -> (
        match project (full_eval t) with
        | Some Zero -> inject Zero
        | Some (Succ n) -> n
        | _ -> inject (Pred (full_eval t)))

  let parse ~inject ~p ~full_parser =
    match Input.peek_token p with
    | "zero" ->
        ignore (Input.consume_token p);
        Some (inject Zero)
    | "succ" -> (
        ignore (Input.consume_token p);
        match full_parser p with
        | Some t -> Some (inject (Succ t))
        | None -> None)
    | "pred" -> (
        ignore (Input.consume_token p);
        match full_parser p with
        | Some t -> Some (inject (Pred t))
        | None -> None)
    | _ -> None

  let pp ~full_pp = function
    | Zero -> "0"
    | Succ n -> Printf.sprintf "succ(%s)" (full_pp n)
    | Pred n -> Printf.sprintf "pred(%s)" (full_pp n)

  let fmap ~f = function
    | Zero -> Zero
    | Succ t -> Succ (f t)
    | Pred t -> Pred (f t)
end

module TypedNatFragment : TYPED_FRAGMENT = struct
  include NatFragment

  type 'b ty = Nat

  let get_type ~ctx:_ ~project ~inject ~full_get_type = function
    | Zero -> Some (inject Nat)
    | Succ t -> (
        match project (full_get_type t) with
        | Some Nat -> Some (inject Nat)
        | _ -> None)
    | Pred t -> (
        match project (full_get_type t) with
        | Some Nat -> Some (inject Nat)
        | _ -> None)

  let pp_ty Nat = "Nat"
end
