open Import
open Fiber.O
module Trigger = Fiber.Trigger

type step =
  run_id:Run_id.t
  -> restart_started_at:Time.t option
  -> (unit, [ `Already_reported ]) Result.t Fiber.t

type status =
  | (* We are not doing a build. Just accumulating invalidations until the next
       build starts. *)
    Standing_by
  | (* Running a build *) Building
  | (* Cancellation requested. Build jobs are immediately rejected in this
       state *)
    Restarting_build

type t =
  { mutable status : status
  ; mutable invalidation : Memo.Invalidation.t
  ; mutable watch_restart_started_at : Time.t option
  ; mutable build_inputs_changed : Trigger.t
  ; mutable latest_input_change_run_id : Run_id.t option
    (* Set whenever an invalidation is recorded. This lets waiters survive
       another build iteration replacing [build_inputs_changed]. *)
  ; mutable current_run_id : Run_id.t option
  ; build_mutex : Fiber.Mutex.t
  }

let next_run_id = ref 1

let build_finish (build_result : Build_outcome.t) =
  let message =
    match build_result with
    | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
    | Failure ->
      let failure_message =
        match
          Build_system_error.(
            Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
        with
        | 1 -> Pp.textf "Had 1 error"
        | n -> Pp.textf "Had %d errors" n
      in
      Pp.tag User_message.Style.Error failure_message
  in
  Console.Status_line.set
    (Constant (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))
;;

let invalidation_of_file_events events =
  List.fold_left events ~init:Memo.Invalidation.empty ~f:(fun acc event ->
    Memo.Invalidation.combine
      acc
      (match (event : Event.File_watcher_event.t) with
       | Fs_memo_event event -> Fs_memo.handle_fs_event event
       | Queue_overflow -> Memo.Invalidation.clear_caches ~reason:Event_queue_overflow))
;;

let show_build_interrupted_status_line () =
  Console.Status_line.set
    (Live
       (fun () ->
         let progress =
           match Fiber.Svar.read Build_system.state with
           | Initializing
           | Restarting_current_build
           | Build_succeeded__now_waiting_for_changes
           | Build_failed__now_waiting_for_changes -> Build_system.Progress.init
           | Building progress -> progress
         in
         Pp.seq
           (Pp.tag User_message.Style.Error (Pp.verbatim "Source files changed"))
           (Pp.verbatim
              (sprintf
                 ", restarting current build... (%u/%u)"
                 progress.number_of_rules_executed
                 progress.number_of_rules_discovered))))
;;

let current_run_id t =
  match t.current_run_id with
  | Some run_id -> run_id
  | None -> Code_error.raise "expected current build run id" []
;;

let request_restart t invalidation =
  let* () = Fiber.return () in
  if Memo.Invalidation.is_empty invalidation
  then Fiber.return ()
  else (
    let now = Time.now () in
    let input_change_run_id =
      match t.status with
      | Standing_by ->
        (* Idle changes are represented by [restart = true] on the next
           build-start event. [build-restart] is emitted only when we cancel an
           active build below. *)
        Run_id.Watch !next_run_id
      | Building | Restarting_build -> current_run_id t
    in
    if Option.is_none t.watch_restart_started_at
    then t.watch_restart_started_at <- Some now;
    t.invalidation <- Memo.Invalidation.combine t.invalidation invalidation;
    t.latest_input_change_run_id <- Some input_change_run_id;
    let* () =
      match t.status with
      | Restarting_build | Standing_by -> Fiber.return ()
      | Building ->
        show_build_interrupted_status_line ();
        t.status <- Restarting_build;
        let+ () = Scheduler.cancel_current_build () in
        Dune_trace.emit Build (fun () ->
          Dune_trace.Event.watch_build_restart
            ~run_id:(Run_id.to_int input_change_run_id)
            ~reasons:(Memo.Invalidation.details_hum ~max_elements:max_int invalidation)
            ~at:now)
    in
    Trigger.trigger t.build_inputs_changed)
;;

let rec handle_file_events t file_watcher =
  File_watcher.read file_watcher
  >>= function
  | None ->
    Scheduler.shutdown ();
    Fiber.return ()
  | Some events ->
    let* () = request_restart t (invalidation_of_file_events events) in
    handle_file_events t file_watcher
;;

let run f =
  let t =
    { status = Standing_by
    ; invalidation = Memo.Invalidation.empty
    ; watch_restart_started_at = None
    ; build_inputs_changed = Trigger.create ()
    ; latest_input_change_run_id = None
    ; current_run_id = None
    ; build_mutex = Fiber.Mutex.create ()
    }
  in
  match Scheduler.file_watcher () with
  | None ->
    ignore (Fs_memo.init ~dune_file_watcher:None : Memo.Invalidation.t);
    f t
  | Some file_watcher ->
    Fs_memo.init ~dune_file_watcher:(Some file_watcher) |> Memo.reset;
    Fiber.fork_and_join_unit
      (fun () -> handle_file_events t file_watcher)
      (fun () ->
         Fiber.finalize
           (fun () -> f t)
           ~finally:(fun () ->
             File_watcher.close file_watcher;
             Fiber.return ()))
;;

let run_current_build t ~run_id step =
  let restart_started_at = t.watch_restart_started_at in
  t.invalidation <- Memo.Invalidation.empty;
  let build_start_input_change_run_id = t.latest_input_change_run_id in
  let previous_build_inputs_changed = t.build_inputs_changed in
  t.build_inputs_changed <- Trigger.create ();
  (* Wake waiters on the previous trigger generation. We replace the trigger at
     the start of every build so that waiters created during this build observe
     only changes that happen after the build started. *)
  let* () = Trigger.trigger previous_build_inputs_changed in
  (match t.status with
   | Building -> assert false
   | Standing_by | Restarting_build -> ());
  t.status <- Building;
  t.current_run_id <- Some run_id;
  let+ outcome =
    Scheduler.with_current_build_cancellation (Fiber.Cancel.create ()) (fun () ->
      step ~run_id ~restart_started_at)
  in
  let next =
    match t.status with
    | Restarting_build -> `Restart
    | Standing_by | Building ->
      t.status <- Standing_by;
      t.current_run_id <- None;
      t.watch_restart_started_at <- None;
      `Done
  in
  outcome, next, build_start_input_change_run_id
;;

let rec poll_iter_locked t step =
  let run_id =
    let run_id = Run_id.Watch !next_run_id in
    incr next_run_id;
    run_id
  in
  let invalidation = t.invalidation in
  if Memo.Invalidation.is_empty invalidation
  then Memo.Metrics.reset ()
  else (
    let details_hum = Memo.Invalidation.details_hum invalidation in
    Console.maybe_clear_screen ~details_hum;
    Memo.reset invalidation);
  let* res, next, build_start_input_change_run_id = run_current_build t ~run_id step in
  match next with
  | `Restart -> poll_iter_locked t step
  | `Done ->
    let res : Build_outcome.t =
      match res with
      | Error `Already_reported -> Failure
      | Ok () -> Success
    in
    build_finish res;
    Fiber.return (res, build_start_input_change_run_id)
;;

let poll_iter t step =
  Fiber.Mutex.with_lock t.build_mutex ~f:(fun () -> poll_iter_locked t step)
;;

(* Work we're allowed to do between successive polling iterations. this work
   should be fast and never fail (within reason) *)
let run_when_idle () : unit =
  Dune_trace.emit ~buffered:true Scheduler Dune_trace.Event.scheduler_idle;
  Dune_trace.flush ()
;;

let rec wait_for_build_input_change_after t input_change_run_id =
  let* () = Fiber.return () in
  if not (Option.equal Run_id.equal t.latest_input_change_run_id input_change_run_id)
  then Fiber.return ()
  else (
    let trigger = t.build_inputs_changed in
    let* () = Trigger.wait trigger in
    wait_for_build_input_change_after t input_change_run_id)
;;

let poll t step =
  let rec loop () =
    let* (_ : Build_outcome.t), input_change_run_id = poll_iter t step in
    run_when_idle ();
    let* () = wait_for_build_input_change_after t input_change_run_id in
    loop ()
  in
  loop ()
;;

let poll_passive t ~get_build_request =
  let rec loop () =
    let* step, response_ivar = get_build_request in
    (* Flush before to make the build reproducible. The passive watch mode is
       designed for tests and We want to observe all the change made by the
       test before starting the build. *)
    let* () = Scheduler.flush_file_watcher () in
    let* res, (_input_change_run_id : Run_id.t option) = poll_iter t step in
    let* () = Fiber.Ivar.fill response_ivar res in
    loop ()
  in
  loop ()
;;

module For_tests = struct
  let wait_for_build_input_change t = Trigger.wait t.build_inputs_changed
  let inject_memo_invalidation = request_restart
end
