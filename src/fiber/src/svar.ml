open Stdune
open Core
open Core.O

type 'a t =
  { mutable current : 'a
  ; mutable waiters : (unit k * ('a -> bool)) list
  }

let read t = t.current

let wait =
  let suspend t ~until = suspend (fun k -> t.waiters <- (k, until) :: t.waiters) in
  let rec wait t ~until =
    if until t.current
    then return ()
    else
      let* () = suspend t ~until in
      wait t ~until
  in
  fun t ~until -> wait t ~until
;;

let create current = { current; waiters = [] }

let run_write t a k =
  t.current <- a;
  let sleep, awake =
    List.rev_partition_map t.waiters ~f:(fun (k, f) ->
      if f t.current then Right k else Left (k, f))
  in
  match awake with
  | [] -> continue k ()
  | awake ->
    t.waiters <- List.rev sleep;
    continue (Resume_many (awake, k)) ()
;;

let write t a = primitive2 run_write t a
