open Fragment

module FnFragment : FRAGMENT = struct
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
    | "abs" -> (
        ignore (Input.consume_token p);
        match full_parser p with
        | Some t -> Some (inject (Abstraction t))
        | None -> None)
    | "app" -> (
        ignore (Input.consume_token p);
        let o1 = full_parser p in
        Input.skip_ws p;
        let o2 = full_parser p in
        match (o1, o2) with
        | Some t1, Some t2 -> Some (inject (Application (t1, t2)))
        | _ -> None)
    | "var" -> (
        ignore (Input.consume_token p);
        match Input.int_token p with
        | Some i -> Some (inject (Var i))
        | None -> None)
    | _ -> None

  let pp ~full_pp = function
    | Var i -> string_of_int i
    | Abstraction body -> Printf.sprintf "\xce\xbb. %s" (full_pp body)
    | Application (f, a) -> Printf.sprintf "(%s %s)" (full_pp f) (full_pp a)

  let fmap ~f = function
    | Var i -> Var i
    | Abstraction body -> Abstraction (f body)
    | Application (fn, a) -> Application (f fn, f a)
end
