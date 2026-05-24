open TypedFragment
open ParseResult

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
        Input.swallow_token p;
        Ok (inject Zero)
    | "succ" ->
        Input.swallow_token p;
        let* t = full_parser p in
        Ok (inject (Succ t))
    | "pred" ->
        Input.swallow_token p;
        let* t = full_parser p in
        Ok (inject (Pred t))
    | _ -> Skip

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

  let get_type ~ctx ~project ~inject ~full_get_type = function
    | Zero -> Some (inject Nat)
    | Succ t ->
        Option.bind (full_get_type ctx t) (fun ty ->
            Option.bind (project ty) (function Nat -> Some (inject Nat)))
    | Pred t ->
        Option.bind (full_get_type ctx t) (fun ty ->
            Option.bind (project ty) (function Nat -> Some (inject Nat)))

  let pp_ty ~full_pp:_ Nat = "Nat"
end
