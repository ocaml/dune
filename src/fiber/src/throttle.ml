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

(* Jobs start in the order in which they arrive. A job at the head of the
   queue that does not fit blocks all the jobs behind it, so that a heavy job
   does not wait forever behind a stream of light jobs. *)
let rec restart t =
  match Queue.peek t.waiting with
  | Some { weight; ivar } when t.running + weight <= t.size ->
    ignore (Queue.pop_exn t.waiting);
    t.running <- t.running + weight;
    let* () = Ivar.fill ivar () in
    restart t
  | Some _ | None -> return ()
;;

let resize t n =
  t.size <- n;
  restart t
;;

let run_weighted t ~weight ~f =
  let weight = min weight t.size in
  finalize
    ~finally:(fun () ->
      t.running <- t.running - weight;
      restart t)
    (fun () ->
       if Queue.is_empty t.waiting && t.running + weight <= t.size
       then (
         t.running <- t.running + weight;
         f ())
       else (
         let ivar = Ivar.create () in
         Queue.push t.waiting { weight; ivar };
         let* () = Ivar.read ivar in
         f ()))
;;

let run t ~f = run_weighted t ~weight:1 ~f
