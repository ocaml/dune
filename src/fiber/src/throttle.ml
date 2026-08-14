open Stdune
open Core
open Core.O

type priority = unit Ivar.t Priority_queue.priority

type t =
  { mutable size : int
  ; mutable running : int
  ; waiting : unit Ivar.t Priority_queue.t
  }

let create size = { size; running = 0; waiting = Priority_queue.create () }
let size t = t.size
let running t = t.running

let rec restart_waiters t acc =
  if t.running >= t.size
  then List.rev acc
  else (
    match Priority_queue.pop t.waiting with
    | None -> List.rev acc
    | Some ivar ->
      t.running <- t.running + 1;
      restart_waiters t (ivar :: acc))
;;

let restart_waiters t = restart_waiters t []

let restart t =
  of_thunk (fun () ->
    restart_waiters t |> sequential_iter ~f:(fun ivar -> Ivar.fill ivar ()))
;;

let resize t n =
  t.size <- n;
  restart t
;;

let create_priority ?priority t = Priority_queue.create_priority ?priority t.waiting
let increase_priority = Priority_queue.increase_priority
let increase_priority_by = Priority_queue.increase_priority_by

let restart_after_job t priority schedule_restart =
  match schedule_restart, priority, Priority_queue.max_priority t.waiting with
  | Some schedule_restart, Some priority, Some waiting_priority
    when Priority_queue.priority priority > waiting_priority ->
    schedule_restart ();
    return ()
  | _ -> restart t
;;

let run t ?priority ?schedule_restart f =
  of_thunk (fun () ->
    Option.iter priority ~f:(Priority_queue.check_priority t.waiting);
    let* () =
      if t.running < t.size && Priority_queue.is_empty t.waiting
      then (
        t.running <- t.running + 1;
        return ())
      else (
        let priority =
          match priority with
          | Some priority -> priority
          | None -> create_priority t
        in
        let waiting = Ivar.create () in
        Priority_queue.push t.waiting priority waiting;
        let* () = restart t in
        Ivar.read waiting)
    in
    finalize
      ~finally:(fun () ->
        t.running <- t.running - 1;
        restart_after_job t priority schedule_restart)
      f)
;;
