type 'a t = int -> 'a option

let empty () = fun _n -> None
let extend ctx x = function 0 -> Some x | n -> ctx (n - 1)
