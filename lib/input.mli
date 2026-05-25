type t

(* Maker methods *)

val from_string : string -> t
(** Create an input from a string*)

val from_file : string -> t
(** Create an input from a file*)

val from_stdin : unit -> t
(** Create an input from stdin*)

(* Token manipulation methods *)

val peek_token : t -> string
(** Peek (but not consume) the next token *)

val expect_token : t -> string -> bool
(** Expect a given token as the next token. If succeeds, *)

val swallow_token : t -> unit
(** Swallow and throw away the next token*)

val int_token : t -> int option
(** Read an integer *)

(** Methods for error handling *)

val pos : t -> int
val print_error : t -> ?pos:int -> ?msg:string -> unit -> unit

(* Exposed for convenience, don't use unless no other way *)

val skip_ws : t -> unit
(** Skip to next non-ws character *)

val parse_parend : f:(t -> 'a) -> t -> 'a
