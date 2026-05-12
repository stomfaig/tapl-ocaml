type ('a, 'e) parse_results = Ok of 'a | Failed of 'e | Skip

let ( let* ) r f = match r with Ok v -> f v | e -> e
let skip_to r f = match r with Skip -> f () | v -> v

type parse_error = { pos : int; msg : string }
