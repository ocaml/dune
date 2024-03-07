open Stdune
open Fiber.O

type 'a t =
  { mutable status : [ `Open | `Closed ]
  ; readers : [ `Closed | `Next of 'a ] Fiber.Ivar.t Queue.t
  ; writers : ([ `Closed | `Ok ] Fiber.Ivar.t * 'a) Queue.t
  }

let create () = { status = `Open; readers = Queue.create (); writers = Queue.create () }

let or_closed t f =
  match t.status with
  | `Open -> f ()
  | `Closed -> Fiber.return `Closed
;;

let push t event =
  let* () = Fiber.return () in
  or_closed t (fun () ->
    match Queue.pop t.readers with
    | Some reader -> Fiber.Ivar.fill reader (`Next event) >>> Fiber.return `Ok
    | None ->
      let ivar = Fiber.Ivar.create () in
      Queue.push t.writers (ivar, event);
      Fiber.Ivar.read ivar)
;;

let rec pop_all q ~f =
  match Queue.pop q with
  | None -> Fiber.return ()
  | Some e ->
    let* () = f e in
    pop_all q ~f
;;

let close t =
  let* () = Fiber.return () in
  match t.status with
  | `Closed -> Fiber.return ()
  | `Open ->
    t.status <- `Closed;
    (* only one of these should be full in reality *)
    Fiber.fork_and_join_unit
      (fun () -> pop_all t.readers ~f:(fun reader -> Fiber.Ivar.fill reader `Closed))
      (fun () -> pop_all t.writers ~f:(fun (writer, _) -> Fiber.Ivar.fill writer `Closed))
;;

let pop t =
  let* () = Fiber.return () in
  or_closed t (fun () ->
    match Queue.pop t.writers with
    | Some (ivar, event) -> Fiber.Ivar.fill ivar `Ok >>> Fiber.return (`Next event)
    | None ->
      let ivar = Fiber.Ivar.create () in
      Queue.push t.readers ivar;
      Fiber.Ivar.read ivar)
;;
