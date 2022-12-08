open Stdune
open Core
open Core.O

type t =
  { mutable size : int
  ; mutable running : int
  ; waiting : unit Ivar.t Queue.t
  }

let create size = { size; running = 0; waiting = Queue.create () }

let size t = t.size

let running t = t.running

let rec restart t =
  if t.running >= t.size then return ()
  else
    match Queue.pop t.waiting with
    | None -> return ()
    | Some ivar ->
      t.running <- t.running + 1;
      let* () = Ivar.fill ivar () in
      restart t

let resize t n =
  t.size <- n;
  restart t

let run t ~f =
  finalize
    ~finally:(fun () ->
      t.running <- t.running - 1;
      restart t)
    (fun () ->
      if t.running < t.size then (
        t.running <- t.running + 1;
        f ())
      else
        let waiting = Ivar.create () in
        Queue.push t.waiting waiting;
        let* () = Ivar.read waiting in
        f ())
