open Fragment

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
    'a node ->
    'b option
  (** Infer the type of a node.
      - [project] extracts this fragment's type tag from a combined type value.
      - [inject] wraps a type tag into the combined type value.
      - [full_get_type] infers the type of an arbitrary combined sub-term. *)

  val pp_ty : full_pp:('b -> string) -> 'b ty -> string
  (** Pretty-print a type tag. *)
end

(** Open-recursive combinator for typed fragments.

    Unlike {!TypedCombine}, the result is itself a {!TYPED_FRAGMENT} and can be
    nested further before being closed with {!TypedTie}. *)
module TypedUntiedCombine (F1 : TYPED_FRAGMENT) (F2 : TYPED_FRAGMENT) = struct
  include Fragment.UntiedCombine (F1) (F2)

  type 'b ty = TL of 'b F1.ty | TR of 'b F2.ty

  let get_type ~ctx ~project ~inject ~full_get_type = function
    | L n ->
        let project' b =
          match project b with Some (TL t) -> Some t | _ -> None
        in
        let inject' t = inject (TL t) in
        F1.get_type ~ctx ~project:project' ~inject:inject' ~full_get_type n
    | R n ->
        let project' b =
          match project b with Some (TR t) -> Some t | _ -> None
        in
        let inject' t = inject (TR t) in
        F2.get_type ~ctx ~project:project' ~inject:inject' ~full_get_type n

  let pp_ty ~full_pp = function
    | TL t -> F1.pp_ty ~full_pp t
    | TR t -> F2.pp_ty ~full_pp t
end

(** Combine two typed fragments, merging their type systems.

    The combined type is [TL of ty F1.ty | TR of ty F2.ty], mirroring the
    [L]/[R] split of the term type. *)
module TypedCombine (F1 : TYPED_FRAGMENT) (F2 : TYPED_FRAGMENT) = struct
  include Combine (F1) (F2)

  type ty = TL of ty F1.ty | TR of ty F2.ty

  let inject_ty_l t = TL t
  let inject_ty_r t = TR t
  let project_ty_l = function TL t -> Some t | _ -> None
  let project_ty_r = function TR t -> Some t | _ -> None

  (** Infer the type of a combined term. Raises [Failure] if a sub-term is
      ill-typed; callers should catch if partial type information is needed. *)
  let get_type term =
    let rec full_get_type ctx term =
      match term with
      | L n ->
          F1.get_type ~ctx ~project:project_ty_l ~inject:inject_ty_l
            ~full_get_type n
      | R n ->
          F2.get_type ~ctx ~project:project_ty_r ~inject:inject_ty_r
            ~full_get_type n
    in
    let ctx = Context.empty () in
    full_get_type ctx term

  (** Pretty-print a combined type. *)
  let rec pp_ty = function
    | TL t -> F1.pp_ty ~full_pp:pp_ty t
    | TR t -> F2.pp_ty ~full_pp:pp_ty t
end

(** Tie the recursive knot on a single typed fragment. *)
module TypedTie (F : TYPED_FRAGMENT) = struct
  include Tie (F)

  type ty = Ty of ty F.ty

  let inject_ty t = Ty t
  let project_ty (Ty t) = Some t

  let get_type term =
    let rec full_get_type ctx (In n) =
      F.get_type ~ctx ~project:project_ty ~inject:inject_ty ~full_get_type n
    in
    full_get_type (Context.empty ()) term

  let rec pp_ty (Ty t) = F.pp_ty ~full_pp:pp_ty t
end
