open Stdune
open Core
open Core.O

type t =
  { mutable locked : bool
  ; mutable waiters : unit k Queue.t
  }

let lock t k =
  if t.locked then suspend (fun k -> Queue.push t.waiters k) k
  else (
    t.locked <- true;
    k ())

let unlock t k =
  assert t.locked;
  match Queue.pop t.waiters with
  | None ->
    t.locked <- false;
    k ()
  | Some next -> resume next () k

let with_lock t ~f =
  let* () = lock t in
  finalize f ~finally:(fun () -> unlock t)

let create () = { locked = false; waiters = Queue.create () }
