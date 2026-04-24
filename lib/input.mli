type t

exception InputError

val from_string : string -> t
val from_file : string -> t
val from_stdin : unit -> t
val peek : t -> char option
val advance : t -> unit
val skip_ws : t -> unit
val peek_token : t -> string
val consume_token : t -> string
val int_token : t -> int option
val expect : t -> char -> bool
val expect_str : t -> string -> bool
