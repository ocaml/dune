open Stdune
open Core
open Core.O

type t =
  { mutable locked : bool
  ; mutable waiters : unit k Queue.t
  }

let run_lock t k =
  if t.locked
  then Suspend ((fun k -> Queue.push t.waiters k), k)
  else (
    t.locked <- true;
    continue k ())
;;

let lock t = primitive run_lock t

let run_unlock t k =
  assert t.locked;
  match Queue.pop t.waiters with
  | None ->
    t.locked <- false;
    continue k ()
  | Some next -> Resume (next, (), k)
;;

let unlock t = primitive run_unlock t

let with_lock t ~f =
  let* () = lock t in
  finalize f ~finally:(fun () -> unlock t)
;;

let create () = { locked = false; waiters = Queue.create () }
