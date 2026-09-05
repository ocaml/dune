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
  ; waiting : waiter Queue.t
  }

and slot =
  { throttle : t
  ; weight : int
  ; mutable state : state
  }

and state =
  | Released
  | Waiting of slot_waiter
  | Acquired

and waiter =
  | Anonymous of waiting
  | Slot of slot_waiter

and slot_waiter =
  { slot : slot
  ; ready : bool Ivar.t
  ; mutable cancelled : bool
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
    | Some (Slot waiter) when waiter.cancelled -> restart t
    | Some (Anonymous { weight; ivar }) ->
      t.running <- t.running + weight;
      let* () = Ivar.fill ivar () in
      restart t
    | Some (Slot waiter) ->
      t.running <- t.running + waiter.slot.weight;
      waiter.slot.state <- Acquired;
      let* () = Ivar.fill waiter.ready true in
      restart t)
;;

let resize t n =
  t.size <- n;
  restart t
;;

module Slot = struct
  type t = slot

  let create throttle ~size =
    { throttle; weight = min size throttle.size; state = Released }
  ;;

  let acquire t =
    let* () = return () in
    match t.state with
    | Acquired -> return true
    | Waiting waiter -> Ivar.read waiter.ready
    | Released ->
      let throttle = t.throttle in
      if Queue.is_empty throttle.waiting && throttle.running < throttle.size
      then (
        throttle.running <- throttle.running + t.weight;
        t.state <- Acquired;
        return true)
      else (
        let waiter = { slot = t; ready = Ivar.create (); cancelled = false } in
        t.state <- Waiting waiter;
        Queue.push throttle.waiting (Slot waiter);
        Ivar.read waiter.ready)
  ;;

  let release t =
    let* () = return () in
    let state = t.state in
    t.state <- Released;
    match state with
    | Released -> return ()
    | Acquired ->
      t.throttle.running <- t.throttle.running - t.weight;
      restart t.throttle
    | Waiting waiter ->
      waiter.cancelled <- true;
      let* () = Ivar.fill waiter.ready false in
      restart t.throttle
  ;;
end

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
         Queue.push t.waiting (Anonymous { weight; ivar });
         let* () = Ivar.read ivar in
         f ()))
;;
