open TypedFragment
open ParseResult
open TypingState

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
        Input.swallow_token p;
        Ok (inject True)
    | "false" ->
        Input.swallow_token p;
        Ok (inject False)
    | "if" ->
        Input.swallow_token p;
        let* t1 = full_parser p in
        if not (Input.expect_token p "then") then
          Failed
            {
              pos = Input.pos p;
              msg = "Failed parsing if block: \"then\" expected";
            }
        else
          let* t2 = full_parser p in
          if not (Input.expect_token p "else") then
            Failed
              {
                pos = Input.pos p;
                msg = "Failed parsing if block: \"else\" expected";
              }
          else
            let* t3 = full_parser p in
            Ok (inject (If (t1, t2, t3)))
    | _ -> Skip

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

  let get_type ~ctx ~project ~inject ~full_get_type ~annot:_ = function
    | True | False -> Some (inject Bool)
    | If (t1, t2, t3) ->
        Option.bind (full_get_type ctx t1) (fun ty ->
            Option.bind (project ty) (function Bool ->
                let t2_ty = full_get_type ctx t2 in
                if t2_ty = full_get_type ctx t3 then t2_ty else None))

  let pp_ty ~full_pp:_ Bool = "Bool"

  let parse_ty ~p ~full_parse_ty:_ =
    match Input.peek_token p with
    | "Bool" ->
        Input.swallow_token p;
        Annotated Bool
    | _ -> Error
end
