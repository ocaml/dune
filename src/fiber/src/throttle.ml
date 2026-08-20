open Stdune
open Core
open Core.O

type t =
  { mutable size : int
  ; mutable running : int
  ; mutable reserved : int
  ; waiting : unit Ivar.t Priority_queue.t
  }

and priority =
  { queue_priority : unit Ivar.t Priority_queue.priority
  ; mutable restart_blockers : int
  ; mutable blocked_restarts : restart list
  }

and restart =
  { throttle : t
  ; priority : priority
  ; mutable state : restart_state
  }

and restart_state =
  | Scheduled
  | Blocked
  | Released

and restart_blocker =
  { priority : priority
  ; mutable released : bool
  }

let create size = { size; running = 0; reserved = 0; waiting = Priority_queue.create () }
let size t = t.size
let running t = t.running
let occupied t = t.running + t.reserved

let rec admit_waiters t acc =
  if occupied t >= t.size
  then List.rev acc
  else (
    match Priority_queue.pop t.waiting with
    | None -> List.rev acc
    | Some ivar ->
      t.running <- t.running + 1;
      admit_waiters t (ivar :: acc))
;;

let admit_waiters t = admit_waiters t []

let restart t =
  of_thunk (fun () ->
    admit_waiters t |> sequential_iter ~f:(fun ivar -> Ivar.fill ivar ()))
;;

let resize t n =
  t.size <- n;
  restart t
;;

let create_priority ?priority t =
  { queue_priority = Priority_queue.create_priority ?priority t.waiting
  ; restart_blockers = 0
  ; blocked_restarts = []
  }
;;

let set_priority t priority = Priority_queue.set_priority t.queue_priority priority
let increase_priority t = Priority_queue.increase_priority t.queue_priority
let increase_priority_by t by = Priority_queue.increase_priority_by t.queue_priority by

let create_restart_blocker priority =
  if priority.restart_blockers = Int.max_int
  then Code_error.raise "Fiber.Throttle restart blocker overflow" [];
  priority.restart_blockers <- priority.restart_blockers + 1;
  { priority; released = false }
;;

let release_restart_blocker blocker =
  if blocker.released
  then Code_error.raise "Fiber.Throttle restart blocker released twice" [];
  blocker.released <- true;
  let priority = blocker.priority in
  priority.restart_blockers <- priority.restart_blockers - 1;
  if priority.restart_blockers < 0
  then Code_error.raise "Fiber.Throttle negative restart blocker count" [];
  if priority.restart_blockers > 0
  then []
  else (
    let restarts = List.rev priority.blocked_restarts in
    priority.blocked_restarts <- [];
    List.map restarts ~f:(fun restart ->
      match restart.state with
      | Blocked ->
        restart.state <- Scheduled;
        restart
      | Scheduled | Released ->
        Code_error.raise "Fiber.Throttle invalid blocked restart state" []))
;;

let release_restart restart =
  let throttle = restart.throttle in
  restart.state <- Released;
  throttle.reserved <- throttle.reserved - 1;
  if throttle.reserved < 0
  then Code_error.raise "Fiber.Throttle negative reservation count" [];
  admit_waiters throttle
;;

let restart_waiters restart =
  match restart.state with
  | Blocked | Released -> Code_error.raise "Fiber.Throttle restart processed twice" []
  | Scheduled ->
    let priority = restart.priority in
    if priority.restart_blockers > 0
    then (
      restart.state <- Blocked;
      priority.blocked_restarts <- restart :: priority.blocked_restarts;
      `Blocked)
    else `Ready (release_restart restart)
;;

let restart_after_job t priority schedule_restart =
  match schedule_restart, priority, Priority_queue.max_priority t.waiting with
  | Some schedule_restart, Some priority, Some waiting_priority
    when Priority_queue.priority priority.queue_priority > waiting_priority ->
    t.running <- t.running - 1;
    t.reserved <- t.reserved + 1;
    let restart = { throttle = t; priority; state = Scheduled } in
    (match schedule_restart restart with
     | () -> return ()
     | exception exn ->
       let waiters = release_restart restart in
       let* () = sequential_iter waiters ~f:(fun ivar -> Ivar.fill ivar ()) in
       raise exn)
  | _ ->
    t.running <- t.running - 1;
    restart t
;;

let run t ?priority ?schedule_restart f =
  of_thunk (fun () ->
    Option.iter priority ~f:(fun priority ->
      Priority_queue.check_priority t.waiting priority.queue_priority);
    let* () =
      if occupied t < t.size && Priority_queue.is_empty t.waiting
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
        Priority_queue.push t.waiting priority.queue_priority waiting;
        let* () = restart t in
        Ivar.read waiting)
    in
    finalize ~finally:(fun () -> restart_after_job t priority schedule_restart) f)
;;
