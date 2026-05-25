open Fragment
open TypingState

(** A fragment that additionally provides a type-checking function. Extends
    {!FRAGMENT} with a type representation ['b ty] and a [get_type] that assigns
    types to terms in the open-recursive style. *)
module type TYPED_FRAGMENT = sig
  include FRAGMENT

  type 'b ty
  (** The type-level representation introduced by this fragment. *)

  val get_type :
    ctx:'b Context.t ->
    project:('b -> 'b ty option) ->
    inject:('b ty -> 'b) ->
    full_get_type:('b Context.t -> 'a -> 'b option) ->
    annot:'b typing_state ->
    'a node ->
    'b option
  (** Infer the type of a node.
      - [project] extracts this fragment's type tag from a combined type value.
      - [inject] wraps a type tag into the combined type value.
      - [full_get_type] infers the type of an arbitrary combined sub-term. *)

  val pp_ty : full_pp:('b -> string) -> 'b ty -> string
  (** Pretty-print a type tag. *)

  val parse_ty :
    p:Input.t ->
    full_parse_ty:(Input.t -> 'b typing_state) ->
    'b ty typing_state
  (** Parse a type expression. [full_parse_ty] parses a combined type value
      recursively, used for composite types like arrows. *)
end

(** Open-recursive combinator for typed fragments.

    Unlike {!TypedCombine}, the result is itself a {!TYPED_FRAGMENT} and can be
    nested further before being closed with {!TypedTie}. *)
module TypedUntiedCombine (F1 : TYPED_FRAGMENT) (F2 : TYPED_FRAGMENT) = struct
  include Fragment.UntiedCombine (F1) (F2)

  type 'b ty = TL of 'b F1.ty | TR of 'b F2.ty

  let get_type ~ctx ~project ~inject ~full_get_type ~annot = function
    | L n ->
        let project' b =
          match project b with Some (TL t) -> Some t | _ -> None
        in
        let inject' t = inject (TL t) in
        F1.get_type ~ctx ~project:project' ~inject:inject' ~full_get_type ~annot
          n
    | R n ->
        let project' b =
          match project b with Some (TR t) -> Some t | _ -> None
        in
        let inject' t = inject (TR t) in
        F2.get_type ~ctx ~project:project' ~inject:inject' ~full_get_type ~annot
          n

  let pp_ty ~full_pp = function
    | TL t -> F1.pp_ty ~full_pp t
    | TR t -> F2.pp_ty ~full_pp t

  let parse_ty ~p ~full_parse_ty =
    match F1.parse_ty ~p ~full_parse_ty with
    | Annotated ty -> Annotated (TL ty)
    | _ -> (
        match F2.parse_ty ~p ~full_parse_ty with
        | Annotated ty -> Annotated (TR ty)
        | _ -> Error)
end

(** Tie the recursive knot on a single typed fragment.

    The [term] type carries optional annotations:
    [Annot of ty option * term F.node] for regular terms and [TypeOnly of ty]
    for type expressions written in term position (e.g. [abs Bool body] where
    [Bool] is the argument-type annotation). *)
module TypedTie (F : TYPED_FRAGMENT) = struct
  type ty = Ty of ty F.ty
  type term = Annot of ty typing_state * term F.node

  let inject node = Annot (Waiting, node)
  let project = function Annot (_, n) -> Some n
  let inject_ty t = Ty t
  let project_ty (Ty t) = Some t

  let rec parse_ty_closed p =
    if Input.peek_token p = "*" then (
      Input.swallow_token p;
      Wildcard)
    else map inject_ty (F.parse_ty ~p ~full_parse_ty:parse_ty_closed)

  let try_parse_annotation p =
    Input.skip_ws p;
    if Input.peek_token p = ":" then begin
      Input.swallow_token p;
      parse_ty_closed p
    end
    else Wildcard

  let rec parse p =
    Input.skip_ws p;
    let result =
      Input.parse_parend ~f:(fun t -> F.parse ~inject ~p:t ~full_parser:parse) p
    in
    match result with
    | ParseResult.Ok (Annot (_, n)) ->
        ParseResult.Ok (Annot (try_parse_annotation p, n))
    | other -> other

  let rec full_get_type ctx = function
    | Annot (annot, node) -> (
        let inferred =
          F.get_type ~ctx ~project:project_ty ~inject:inject_ty ~full_get_type
            ~annot node
        in
        match (inferred, annot) with
        | Some ty, Wildcard -> Some ty
        (* TODO: Implement proper wildcard resolution*)
        | Some ty, Annotated _ann_ty ->
            (* if ty = ann_ty then Some ty else None *) Some ty
        | None, Annotated ann_ty -> Some ann_ty
        | _ -> None)

  let get_type term = full_get_type (Context.empty ()) term
  let rec pp_ty (Ty t) = F.pp_ty ~full_pp:pp_ty t
  let full_map f = function Annot (ann, n) -> Annot (ann, F.fmap ~f n)

  let rec eval = function
    | Annot (_, n) -> F.eval ~inject ~project ~full_eval:eval ~full_map n

  let rec pp = function Annot (_, n) -> F.pp ~full_pp:pp n
end

(** Combine two typed fragments into a closed language. *)
module TypedCombine (F1 : TYPED_FRAGMENT) (F2 : TYPED_FRAGMENT) =
  TypedTie (TypedUntiedCombine (F1) (F2))
