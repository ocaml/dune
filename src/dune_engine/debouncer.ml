type token = int

type t =
  { mutable generation : token
  ; mutable pending : bool
  }

let create () = { generation = 0; pending = false }

let push t =
  t.generation <- t.generation + 1;
  t.pending <- true;
  t.generation
;;

let latest t = if t.pending then Some t.generation else None

let take_if_latest t token =
  if token <> t.generation
  then false
  else if t.pending
  then (
    t.pending <- false;
    true)
  else false
;;

let is_pending t = t.pending
