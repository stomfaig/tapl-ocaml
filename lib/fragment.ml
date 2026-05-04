(** Core signatures and functors for the extensible language framework.

    Languages are built by combining {!FRAGMENT} modules via {!Combine}. Each
    fragment contributes a set of term constructors, an evaluator, a parser, and
    a pretty-printer. Typed fragments additionally supply a type-checking
    function. *)

(** Composable language fragment

    The type parameter ['a] is where the actual constructed type is to be
    injected. *)
module type FRAGMENT = sig
  type 'a node
  (** The node type for this fragment. *)

  val eval :
    inject:('a node -> 'a) ->
    project:('a -> 'a node option) ->
    full_eval:('a -> 'a) ->
    full_map:(('a -> 'a) -> 'a -> 'a) ->
    'a node ->
    'a
  (** Evaluate one step.
      - [inject] wraps a 'a node back into 'a.
      - [project] try to project a 'a to 'a node.
      - [full_eval] evaluates an arbitrary combined sub-term to normal form.
      - [full_map] applies a function to all immediate sub-terms of a combined
        term of any other fragment type (used for traversals which must cross
        fragment boundaries). *)

  val parse :
    inject:('a node -> 'a) ->
    p:Input.t ->
    full_parser:(Input.t -> 'a option) ->
    'a option
  (** Parse a term from the input stream. Must use prefix notation (op_name)
      (arg1) ..., and op_name should not prefix any other op_name in the the
      same or any other fragments that are being constructed.
      - [inject] wraps a parsed node into the combined term type.
      - [full_parser] parses an arbitrary combined sub-term. *)

  val pp : full_pp:('a -> string) -> 'a node -> string
  (** Pretty-print a node. [full_pp] prints an arbitrary sub-term. *)

  val fmap : f:('a -> 'a) -> 'a node -> 'a node
  (** The functor map: apply [f] to every immediate ['a]-typed child, leaving
      the node constructor unchanged. Witnesses that ['a node] is a functor in
      ['a]. *)
end

(** Empty fragment as the identity building block for building fragments *)
module EmptyFragment : FRAGMENT = struct
  type 'a node = |

  let eval ~inject:_ ~project:_ ~full_eval:_ ~full_map:_ _ = assert false
  let parse ~inject:_ ~p:_ ~full_parser:_ = None
  let pp ~full_pp:_ _ = assert false
  let fmap ~f:_ _ = assert false
end

(** A fragment that additionally provides a type-checking function. Extends
    {!FRAGMENT} with a type representation ['b ty] and a [get_type] that assigns
    types to terms in the open-recursive style. *)
module type TYPED_FRAGMENT = sig
  include FRAGMENT

  type 'b ty
  (** The type-level representation introduced by this fragment. *)

  val get_type :
    ctx:'c option ->
    project:('b -> 'b ty option) ->
    inject:('b ty -> 'b) ->
    full_get_type:('a -> 'b) ->
    'a node ->
    'b option
  (** Infer the type of a node.
      - [project] extracts this fragment's type tag from a combined type value.
      - [inject] wraps a type tag into the combined type value.
      - [full_get_type] infers the type of an arbitrary combined sub-term. *)

  val pp_ty : 'b ty -> string
  (** Pretty-print a type tag. *)
end

(* todo: we would like to explicitly annotate this with FRAGMENT
  but also expose the L and R type constructors *)

(** Combine two fragments into a new fragment

    The tricky bit here is that the combined fragments must see the final type
    as their direct ancestor. E.g., consider the sum (Nat + Bool) + Fn. In this
    case, if Bool would see Nat + Bool as it direct ancestor, then we could not
    parse / evaluate things like `if true then (Abs ...) else (Abs ...)` since
    Bool terms could only parse Nat + Bool subterms.

    Therefore, {!F1} and {!F2} are "tricked into thinking" that their direct
    ancestor is 'a. *)
module UntiedCombine (F1 : FRAGMENT) (F2 : FRAGMENT) = struct
  type 'a node = L of 'a F1.node | R of 'a F2.node

  let local_inject_l n = L n
  let local_inject_r n = R n
  let local_project_l = function L n -> Some n | _ -> None
  let local_project_r = function R n -> Some n | _ -> None

  let fmap ~f = function
    | L f1_node -> local_inject_l (F1.fmap ~f f1_node)
    | R f2_node -> local_inject_r (F2.fmap ~f f2_node)

  let eval ~inject ~project ~full_eval ~full_map node =
    let inject_l n = inject (local_inject_l n) in
    let inject_r n = inject (local_inject_r n) in
    let project_l n =
      match project n with Some ln -> local_project_l ln | _ -> None
    in
    let project_r n =
      match project n with Some ln -> local_project_r ln | _ -> None
    in
    match node with
    | L f1_node ->
        F1.eval ~inject:inject_l ~project:project_l ~full_eval ~full_map f1_node
    | R f2_node ->
        F2.eval ~inject:inject_r ~project:project_r ~full_eval ~full_map f2_node

  let parse ~inject ~p ~full_parser =
    let inject_l n = inject (local_inject_l n) in
    let inject_r n = inject (local_inject_r n) in
    match F1.parse ~inject:inject_l ~p ~full_parser with
    | Some _ as term -> term
    | None -> (
        match F2.parse ~inject:inject_r ~p ~full_parser with
        | Some _ as term -> term
        | _ -> None)

  let pp ~full_pp = function L n -> F1.pp ~full_pp n | R n -> F2.pp ~full_pp n
end

module type LANGUAGE = sig
  type term

  val parse : Input.t -> term option
  val eval : term -> term
  val pp : term -> string
end

(** Seal any module satisfying {!LANGUAGE} behind the abstract interface. *)
module Seal (L : LANGUAGE) : LANGUAGE = L

(** Tie the recursive knot on a single fragment, producing a closed language. *)
module Tie (F : FRAGMENT) : LANGUAGE = struct
  type term = In of term F.node

  let inject t = In t
  let project (In t) = Some t
  let rec parse p = F.parse ~inject ~p ~full_parser:parse
  let full_map f (In t) = In (F.fmap ~f t)
  let rec eval (In t) = F.eval ~inject ~project ~full_eval:eval ~full_map t
  let rec pp (In t) = F.pp ~full_pp:pp t
end

(** Combine two fragments into a single language.

    The combined term type is [L of term F1.node | R of term F2.node], which is
    the fixed point of the coproduct functor [F1.node + F2.node]. [eval],
    [parse], and [pp] are closed by threading the combined term type back
    through inject/project/full_eval. *)
module Combine (F1 : FRAGMENT) (F2 : FRAGMENT) = struct
  type term = L of term F1.node | R of term F2.node

  let inject_l n = L n
  let inject_r n = R n
  let project_l = function L n -> Some n | _ -> None
  let project_r = function R n -> Some n | _ -> None

  (** Map a function over all immediate sub-terms of a combined term. *)
  let fmap ~f = function
    | L n -> inject_l (F1.fmap ~f n)
    | R n -> inject_r (F2.fmap ~f n)

  (** Evaluate a combined term to normal form. *)
  let rec eval = function
    | L n ->
        F1.eval ~inject:inject_l ~project:project_l ~full_eval:eval
          ~full_map:(fun f -> fmap ~f)
          n
    | R n ->
        F2.eval ~inject:inject_r ~project:project_r ~full_eval:eval
          ~full_map:(fun f -> fmap ~f)
          n

  (** Parse a combined term; tries [F1] first, then [F2]. This should not cause
      issues, since we assume that Each fragment uses prefix notation. *)
  let rec parse p =
    Input.skip_ws p;
    match F1.parse ~inject:inject_l ~p ~full_parser:parse with
    | Some _ as v -> v
    | None -> (
        match F2.parse ~inject:inject_r ~p ~full_parser:parse with
        | Some _ as v -> v
        | None -> None)

  (** Pretty-print a combined term. *)
  let rec pp = function
    | L n -> F1.pp ~full_pp:pp n
    | R n -> F2.pp ~full_pp:pp n
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
  let rec get_type term =
    let full_get_type t =
      match get_type t with
      | Some ty -> ty
      | None -> failwith "type error in subterm"
    in
    match term with
    | L n ->
        F1.get_type ~ctx:None ~project:project_ty_l ~inject:inject_ty_l
          ~full_get_type n
    | R n ->
        F2.get_type ~ctx:None ~project:project_ty_r ~inject:inject_ty_r
          ~full_get_type n

  (** Pretty-print a combined type. *)
  let pp_ty = function TL t -> F1.pp_ty t | TR t -> F2.pp_ty t
end
