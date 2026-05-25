open ParseResult
open TypingState

module FnFragment = struct
  type 'a node = Var of int | Abstraction of 'a | Application of 'a * 'a

  let eval ~inject ~project ~full_eval ~full_map node =
    let rec sub replacement idx sub_node =
      match project sub_node with
      | Some (Var i) -> if i = idx then replacement else inject (Var i)
      | Some (Abstraction t) ->
          inject (Abstraction (sub replacement (idx + 1) t))
      | Some (Application (f, a)) ->
          inject (Application (sub replacement idx f, sub replacement idx a))
      | None -> full_map (sub replacement idx) sub_node
    in
    match node with
    | Application (f, arg) -> (
        match project (full_eval f) with
        | Some (Abstraction body) -> full_eval (sub (full_eval arg) 0 body)
        | _ -> inject (Application (full_eval f, full_eval arg)))
    | Abstraction _ as v -> inject v
    | Var i -> inject (Var i)

  let parse ~inject ~p ~full_parser =
    match Input.peek_token p with
    | "abs" ->
        Input.swallow_token p;
        let* t = full_parser p in
        Ok (inject (Abstraction t))
    | "app" ->
        Input.swallow_token p;
        let* t1 = full_parser p in
        Input.skip_ws p;
        let* t2 = full_parser p in
        Ok (inject (Application (t1, t2)))
    | "var" -> (
        Input.swallow_token p;
        match Input.int_token p with
        | Some i -> Ok (inject (Var i))
        | None -> Failed { pos = Input.pos p; msg = "Variable number expected" }
        )
    | _ -> Skip

  let pp ~full_pp = function
    | Var i -> string_of_int i
    | Abstraction body -> Printf.sprintf "\xce\xbb. %s" (full_pp body)
    | Application (f, a) -> Printf.sprintf "(%s %s)" (full_pp f) (full_pp a)

  let fmap ~f = function
    | Var i -> Var i
    | Abstraction body -> Abstraction (f body)
    | Application (fn, a) -> Application (f fn, f a)
end

module TyFnFragment = struct
  include FnFragment

  type 'b ty = Callable of ('b * 'b option)

  let get_type ~ctx ~project ~inject ~full_get_type ~annot n =
    match n with
    | Var idx -> ctx idx
    | Abstraction body ->
        begin match annot with
        | Annotated ann_ty ->
            begin match project ann_ty with
            | Some (Callable (arg_ty, _)) ->
                let ctx' = Context.extend ctx arg_ty in
                Option.map
                  (fun ty -> inject (Callable (arg_ty, Some ty)))
                  (full_get_type ctx' body)
            | _ -> None
            end
        | _ -> None
        end
    | Application (t1, t2) ->
        Option.bind (full_get_type ctx t1) (fun ty1 ->
            match project ty1 with
            | Some (Callable (arg_ty, Some ret_ty)) ->
                Option.bind (full_get_type ctx t2) (fun ty2 ->
                    if ty2 = arg_ty then Some ret_ty else None)
            | _ -> None)

  let pp_ty ~full_pp = function
    | Callable (ty1, ty2) ->
        let s1 = full_pp ty1 in
        let s1 =
          if String.contains s1 '>' then Printf.sprintf "(%s)" s1 else s1
        in
        Printf.sprintf "%s -> %s" s1
          (match ty2 with None -> "*" | Some v -> full_pp v)

  let parse_ty ~p ~full_parse_ty =
    match Input.peek_token p with
    | "arr" -> (
        Input.swallow_token p;
        Input.skip_ws p;
        match full_parse_ty p with
        | Annotated t1 ->
            Input.skip_ws p;
            begin match full_parse_ty p with
            | Annotated ty2 -> Annotated (Callable (t1, Some ty2))
            | _ -> Annotated (Callable (t1, None))
            end
        | _ -> Error)
    | _ -> Error
end
