type t = { stream : string; mutable pos : int }

let is_ws = function ' ' | '\t' | '\n' -> true | _ -> false

let peek p =
  if p.pos < String.length p.stream then Some (String.get p.stream p.pos)
  else None

let advance p = p.pos <- p.pos + 1

let rec skip_ws p =
  match peek p with
  | Some c when is_ws c ->
      advance p;
      skip_ws p
  | _ -> ()

let from_string s =
  let p = { stream = s; pos = 0 } in
  skip_ws p;
  p

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

let consume_token p =
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
  skip_ws p;
  Buffer.contents buf

let swallow_token p = ignore (consume_token p)

let peek_token p =
  let saved = p.pos in
  let tok = consume_token p in
  p.pos <- saved;
  tok

let expect_token p s =
  let t = peek_token p in
  if String.equal t s then (
    swallow_token p;
    true)
  else false

let int_token p =
  let saved = p.pos in
  let tok = consume_token p in
  match int_of_string_opt tok with
  | Some _ as n -> n
  | None ->
      p.pos <- saved;
      None

let pos p = p.pos

let print_error p ?(pos = p.pos) ?(msg = "Unrecognised token") () =
  print_endline ("Parsing failed on character " ^ string_of_int pos ^ ":");
  p.pos <- pos;
  let token_length = String.length (peek_token p) in
  let low = max 0 (pos - 10) in
  p.pos <- low;
  let buf = Buffer.create 20 in
  for _i = low to pos + token_length + 5 do
    Buffer.add_char buf (match peek p with Some c -> c | None -> ' ');
    advance p
  done;
  print_endline (Buffer.contents buf);
  let annotation =
    String.make (pos - low) ' ' ^ "^" ^ String.make (token_length - 1) '~'
  in
  print_endline annotation;
  print_endline msg
