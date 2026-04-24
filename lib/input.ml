type t = { stream : string; mutable pos : int }

exception InputError

let is_ws = function ' ' | '\t' | '\n' -> true | _ -> false
let from_string s = { stream = s; pos = 0 }

let from_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  from_string (Bytes.to_string s)

let from_stdin () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf stdin 1
     done
   with End_of_file -> ());
  from_string (Buffer.contents buf)

let peek p =
  if p.pos < String.length p.stream then Some (String.get p.stream p.pos)
  else None

let advance p = p.pos <- p.pos + 1

let expect p c =
  match peek p with
  | Some ch when ch = c ->
      advance p;
      true
  | _ -> false

let expect_str p s =
  let saved = p.pos in
  let ok = ref true in
  String.iter (fun c -> if !ok then ok := expect p c) s;
  if !ok then true
  else (
    p.pos <- saved;
    false)

let rec skip_ws p =
  match peek p with
  | Some c when is_ws c ->
      advance p;
      skip_ws p
  | _ -> ()

let read_token p =
  skip_ws p;
  let buf = Buffer.create 8 in
  let rec collect () =
    match peek p with
    | Some c when not (is_ws c) ->
        advance p;
        Buffer.add_char buf c;
        collect ()
    | _ -> ()
  in
  collect ();
  Buffer.contents buf

let peek_token p =
  let saved = p.pos in
  let tok = read_token p in
  p.pos <- saved;
  tok

let consume_token p = read_token p

let int_token p =
  skip_ws p;
  let saved = p.pos in
  let tok = read_token p in
  match int_of_string_opt tok with
  | Some _ as n -> n
  | None ->
      p.pos <- saved;
      None
