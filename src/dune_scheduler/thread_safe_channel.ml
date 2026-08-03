open Import
open Fiber.O

type 'a item =
  | Value of 'a
  | Fill of Fiber.fill

type 'a t =
  { events : Event.Queue.t
  ; items : 'a item Queue.t
  ; readers : 'a option Fiber.Ivar.t Queue.t
  ; mutex : Mutex.t
  ; mutable closed : bool
  }

let create events =
  { events
  ; items = Queue.create ()
  ; readers = Queue.create ()
  ; mutex = Mutex.create ()
  ; closed = false
  }
;;

let write t value =
  let status, fill =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then `Closed, None
      else (
        match Queue.pop t.readers with
        | Some ivar -> `Ok, Some (Fiber.Fill (ivar, Some value))
        | None ->
          Queue.push t.items (Value value);
          `Ok, None))
  in
  Option.iter fill ~f:(fun fill ->
    Event.Queue.send_worker_tasks_completed t.events [ fill ]);
  status
;;

let write_fill t fill =
  let status, fills =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then `Closed, []
      else if Queue.is_empty t.readers
      then (
        Queue.push t.items (Fill fill);
        `Ok, [])
      else `Ok, [ fill ])
  in
  Event.Queue.send_worker_tasks_completed t.events fills;
  status
;;

let rec read t =
  let* () = Fiber.return () in
  match
    Mutex.protect t.mutex (fun () ->
      match Queue.pop t.items with
      | Some (Value value) -> `Ready (Some value)
      | Some (Fill fill) -> `Fill fill
      | None when t.closed -> `Ready None
      | None ->
        let ivar = Fiber.Ivar.create () in
        Queue.push t.readers ivar;
        `Wait ivar)
  with
  | `Ready value -> Fiber.return value
  | `Fill fill ->
    Event.Queue.send_worker_tasks_completed t.events [ fill ];
    read t
  | `Wait ivar -> Fiber.Ivar.read ivar
;;

let close t =
  let fills =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then []
      else (
        t.closed <- true;
        let rec drain acc =
          match Queue.pop t.readers with
          | None -> List.rev acc
          | Some ivar -> drain (Fiber.Fill (ivar, None) :: acc)
        in
        drain []))
  in
  Event.Queue.send_worker_tasks_completed t.events fills
;;
