type 'a typing_state = Annotated of 'a | Wildcard | Waiting | Error

let map f : 'a -> 'b = function
  | Annotated ty -> Annotated (f ty)
  | Wildcard -> Wildcard
  | Waiting -> Waiting
  | Error -> Error

let bind f : 'a -> 'b typing_state = function
  | Annotated ty -> f ty
  | Wildcard -> Wildcard
  | Waiting -> Waiting
  | Error -> Error
