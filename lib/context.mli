type 'a t = int -> 'a option

val empty : unit -> 'a t
val extend : 'a t -> 'a -> 'a t
