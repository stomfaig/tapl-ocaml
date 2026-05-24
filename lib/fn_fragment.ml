open ParseResult

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
  (* The type annotation on an abstraction is itself a term: the type checker
     infers its type via full_get_type and uses that as the argument type.
     E.g. [abs true body] annotates the argument as Bool because [true : Bool]. *)
  type 'a node =
    | Var of int
    | Abstraction of ('a * 'a)
    | Application of 'a * 'a

  let eval ~inject ~project ~full_eval ~full_map node =
    let rec sub replacement idx sub_node =
      match project sub_node with
      | Some (Var i) -> if i = idx then replacement else inject (Var i)
      | Some (Abstraction (ann, t)) ->
          inject (Abstraction (ann, sub replacement (idx + 1) t))
      | Some (Application (f, a)) ->
          inject (Application (sub replacement idx f, sub replacement idx a))
      | None -> full_map (sub replacement idx) sub_node
    in
    match node with
    | Application (f, arg) -> (
        match project (full_eval f) with
        | Some (Abstraction (_, body)) -> full_eval (sub (full_eval arg) 0 body)
        | _ -> inject (Application (full_eval f, full_eval arg)))
    | Abstraction _ as v -> inject v
    | Var i -> inject (Var i)

  let parse ~inject ~p ~full_parser =
    match Input.peek_token p with
    | "abs" ->
        Input.swallow_token p;
        let* ann = full_parser p in
        Input.skip_ws p;
        let* t = full_parser p in
        Ok (inject (Abstraction (ann, t)))
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
    | Abstraction (ann, body) ->
        Printf.sprintf "\xce\xbb:%s. %s" (full_pp ann) (full_pp body)
    | Application (f, a) -> Printf.sprintf "(%s %s)" (full_pp f) (full_pp a)

  let fmap ~f = function
    | Var i -> Var i
    | Abstraction (ann, body) -> Abstraction (f ann, f body)
    | Application (fn, a) -> Application (f fn, f a)

  type 'b ty = Callable of ('b * 'b)

  let get_type ~ctx ~project ~inject ~full_get_type n =
    match n with
    | Var idx -> ctx idx
    | Abstraction (ann, body) ->
        Option.bind (full_get_type ctx ann) (fun arg_ty ->
            let ctx' = Context.extend ctx arg_ty in
            Option.map
              (fun ret_ty -> inject (Callable (arg_ty, ret_ty)))
              (full_get_type ctx' body))
    | Application (t1, t2) ->
        Option.bind (full_get_type ctx t1) (fun ty1 ->
            match project ty1 with
            | Some (Callable (arg_ty, ret_ty)) ->
                Option.bind (full_get_type ctx t2) (fun ty2 ->
                    if ty2 = arg_ty then Some ret_ty else None)
            | _ -> None)

  let pp_ty ~full_pp = function
    | Callable (ty1, ty2) ->
        let s1 = full_pp ty1 in
        let s1 =
          if String.contains s1 '>' then Printf.sprintf "(%s)" s1 else s1
        in
        Printf.sprintf "%s -> %s" s1 (full_pp ty2)
end
