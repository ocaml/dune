open Stdune
open Core
open O

type 'a t =
  { mutable status : [ `Open | `Closed ]
  ; readers : [ `Closed | `Next of 'a ] Ivar.t Queue.t
  ; writers : ([ `Closed | `Ok ] Ivar.t * 'a) Queue.t
  }

let create () = { status = `Open; readers = Queue.create (); writers = Queue.create () }

let or_closed t f =
  match t.status with
  | `Open -> f ()
  | `Closed -> return `Closed
;;

let push t event =
  let* () = return () in
  or_closed t (fun () ->
    match Queue.pop t.readers with
    | Some reader -> Ivar.fill reader (`Next event) >>> return `Ok
    | None ->
      let ivar = Ivar.create () in
      Queue.push t.writers (ivar, event);
      Ivar.read ivar)
;;

let rec pop_all q ~f =
  match Queue.pop q with
  | None -> return ()
  | Some e ->
    let* () = f e in
    pop_all q ~f
;;

let close t =
  let* () = return () in
  match t.status with
  | `Closed -> return ()
  | `Open ->
    t.status <- `Closed;
    (* only one of these should be full in reality *)
    fork_and_join_unit
      (fun () -> pop_all t.readers ~f:(fun reader -> Ivar.fill reader `Closed))
      (fun () -> pop_all t.writers ~f:(fun (writer, _) -> Ivar.fill writer `Closed))
;;

let pop t =
  let* () = return () in
  or_closed t (fun () ->
    match Queue.pop t.writers with
    | Some (ivar, event) -> Ivar.fill ivar `Ok >>> return (`Next event)
    | None ->
      let ivar = Ivar.create () in
      Queue.push t.readers ivar;
      Ivar.read ivar)
;;
