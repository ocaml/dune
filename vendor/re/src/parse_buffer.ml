type t =
  { str : string
  ; mutable pos : int
  }

exception Parse_error

let create str = { str; pos = 0 }
let unget t = t.pos <- t.pos - 1
let junk t = t.pos <- t.pos + 1
let eos t = t.pos = String.length t.str
let test t c = (not (eos t)) && t.str.[t.pos] = c

let test2 t c c' =
  t.pos + 1 < String.length t.str && t.str.[t.pos] = c && t.str.[t.pos + 1] = c'
;;

let accept t c =
  let r = test t c in
  if r then t.pos <- t.pos + 1;
  r
;;

let accept2 t c c' =
  let r = test2 t c c' in
  if r then t.pos <- t.pos + 2;
  r
;;

let get t =
  let r = t.str.[t.pos] in
  t.pos <- t.pos + 1;
  r
;;

let accept_s t s' =
  let len = String.length s' in
  try
    for j = 0 to len - 1 do
      try if s'.[j] <> t.str.[t.pos + j] then raise_notrace Exit with
      | _ -> raise_notrace Exit
    done;
    t.pos <- t.pos + len;
    true
  with
  | Exit -> false
;;

let rec integer' t i =
  if eos t
  then Some i
  else (
    match get t with
    | '0' .. '9' as d ->
      let i' = (10 * i) + (Char.code d - Char.code '0') in
      if i' < i then raise Parse_error;
      integer' t i'
    | _ ->
      unget t;
      Some i)
;;

let integer t =
  if eos t
  then None
  else (
    match get t with
    | '0' .. '9' as d -> integer' t (Char.code d - Char.code '0')
    | _ ->
      unget t;
      None)
;;
