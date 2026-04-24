open Fragment

module BoolFragment = struct
  type 'a node = If of 'a * 'a * 'a | True | False

  let eval ~inject ~project ~full_eval ~full_map:_ = function
    | (True | False) as v -> inject v
    | If (cond, t, e) as node -> (
        match project (full_eval cond) with
        | Some True -> full_eval t
        | Some False -> full_eval e
        | Some _ -> inject node
        | None -> inject node)

  let parse ~inject ~p ~full_parser =
    match Input.peek_token p with
    | "true" ->
        ignore (Input.consume_token p);
        Some (inject True)
    | "false" ->
        ignore (Input.consume_token p);
        Some (inject False)
    | "if" -> (
        ignore (Input.consume_token p);
        let o1 = full_parser p in
        Input.skip_ws p;
        if not (Input.expect_str p "then") then None
        else
          let o2 = full_parser p in
          Input.skip_ws p;
          if not (Input.expect_str p "else") then None
          else
            let o3 = full_parser p in
            match (o1, o2, o3) with
            | Some t1, Some t2, Some t3 -> Some (inject (If (t1, t2, t3)))
            | _ -> None)
    | _ -> None

  let pp ~full_pp = function
    | True -> "true"
    | False -> "false"
    | If (c, t, e) ->
        Printf.sprintf "if %s then %s else %s" (full_pp c) (full_pp t)
          (full_pp e)

  let fmap ~f = function
    | (True | False) as v -> v
    | If (c, t, e) -> If (f c, f t, f e)
end

module TypedBoolFragment : TYPED_FRAGMENT = struct
  include BoolFragment

  type 'b ty = Bool

  let get_type ~ctx:_ ~project ~inject ~full_get_type = function
    | True | False -> Some (inject Bool)
    | If (t1, t2, t3) -> (
        match project (full_get_type t1) with
        | Some Bool ->
            let t2_ty = full_get_type t2 in
            if t2_ty = full_get_type t3 then Some t2_ty else None
        | _ -> None)

  let pp_ty Bool = "Bool"
end
