type 'a t = { depth : int; entries : (int * 'a) list }

let empty () = { depth = 0; entries = [] }
let inc_depth ctx = { ctx with depth = ctx.depth + 1 }
let dec_depth ctx = { ctx with depth = ctx.depth - 1 }

let get_ty ctx v =
  let real_idx = v - ctx.depth in
  match List.find_opt (fun (idx, _) -> idx = real_idx) ctx.entries with
  | Some (_, ty) -> Some ty
  | None -> None
