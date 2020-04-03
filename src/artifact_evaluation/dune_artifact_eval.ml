type t = string

(* Parse the replacement format described in [artifact_substitution.ml]. *)
let get s =
  let len = String.length s in
  if s.[0] = '=' then
    let colon_pos = String.index_from s 1 ':' in
    let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
    (* This [min] is because the value might have been truncated
       if it was too large *)
    let vlen = min vlen (len - colon_pos - 1) in
    Some (String.sub s (colon_pos + 1) vlen)
  else
    None
[@@inline never]

let encoded x = x
[@@inline never]

let eval s = get s
