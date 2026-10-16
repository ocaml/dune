open Stdune
open Core
open Core.O

type waiting =
  { weight : int
  ; ivar : unit Ivar.t
  }

type t =
  { mutable size : int
  ; mutable running : int
  ; waiting : waiting Queue.t
  }

let create size = { size; running = 0; waiting = Queue.create () }
let size t = t.size
let running t = t.running

let rec restart t =
  if t.running >= t.size
  then return ()
  else (
    match Queue.pop t.waiting with
    | None -> return ()
    | Some { weight; ivar } ->
      t.running <- t.running + weight;
      let* () = Ivar.fill ivar () in
      restart t)
;;

let resize t n =
  t.size <- n;
  restart t
;;

let run t ~size ~f =
  let weight = min size t.size in
  finalize
    ~finally:(fun () ->
      t.running <- t.running - weight;
      restart t)
    (fun () ->
       if Queue.is_empty t.waiting && t.running < t.size
       then (
         t.running <- t.running + weight;
         f ())
       else (
         let ivar = Ivar.create () in
         Queue.push t.waiting { weight; ivar };
         let* () = Ivar.read ivar in
         f ()))
;;
