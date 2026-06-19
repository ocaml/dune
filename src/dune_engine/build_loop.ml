open Import
open Fiber.O
module Trigger = Fiber.Trigger

type status =
  | (* We are not doing a build. Just accumulating invalidations until the next
       build starts. *)
    Standing_by
  | (* Running a build *) Building of Run_id.t
  | (* Cancellation requested. Build jobs are immediately rejected in this
       state *)
    Restarting_build of Run_id.t

module Rpc_request_id = struct
  module T = struct
    type t =
      { session_id : Rpc.Server.Session.Id.t
      ; request_id : Dune_rpc.Id.t
      }

    let repr =
      Repr.record
        "Rpc_request_id.t"
        [ Repr.field "session_id" Repr.int ~get:(fun t ->
            Rpc.Server.Session.Id.to_int t.session_id)
        ; Repr.field "request_id" Dune_rpc.Id.repr ~get:(fun t -> t.request_id)
        ]
    ;;

    include Repr.Poly (struct
        type nonrec t = t

        let repr = repr
      end)

    let create ~session_id ~request_id = { session_id; request_id }
    let session_id t = t.session_id
    let to_dyn = Repr.to_dyn repr
  end

  include T
  module C = Comparable.Make (T)
  module Map = C.Map
end

type t =
  { mutable status : status
  ; mutable pending_reset : Memo.Invalidation.t option
  ; mutable watch_restart_started_at : Time.t option
  ; mutable status_overlay : Console.Status_line.overlay option
  ; mutable wakeup : Trigger.t
  ; mutable wakeup_generation : int
  ; mutable input_change_generation : int
    (* Incremented whenever an invalidation is recorded. This lets waiters
       distinguish input changes from other wakeups. *)
  ; mutable next_run_id : int
  ; mutable rpc_requests : Build_system.Request.Goal.t Rpc_request_id.Map.t
  ; mutable state : [ `Awaiting_init | `Init ]
  }

let clear_status_overlay t =
  Option.iter t.status_overlay ~f:Console.Status_line.remove_overlay;
  t.status_overlay <- None
;;

let set_status_overlay t message =
  clear_status_overlay t;
  t.status_overlay <- Some (Console.Status_line.add_overlay message)
;;

let build_finish t (build_result : Build_outcome.t) =
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
  set_status_overlay
    t
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

let next_watch_run_id t =
  let run_id = Run_id.Watch t.next_run_id in
  t.next_run_id <- t.next_run_id + 1;
  run_id
;;

let trigger_wakeup t =
  t.wakeup_generation <- t.wakeup_generation + 1;
  Trigger.trigger t.wakeup
;;

let add_pending_reset t invalidation =
  t.pending_reset
  <- Some
       (match t.pending_reset with
        | None -> invalidation
        | Some pending -> Memo.Invalidation.combine pending invalidation)
;;

let request_rebuild_due_to_rpc_request t =
  let* () = Fiber.return () in
  let* () =
    match t.status with
    | Restarting_build _ | Standing_by -> Fiber.return ()
    | Building run_id ->
      t.status <- Restarting_build run_id;
      if Option.is_none t.pending_reset
      then t.pending_reset <- Some Memo.Invalidation.empty;
      Process.Build.cancel_current ()
  in
  trigger_wakeup t
;;

let request_restart t invalidation =
  let* () = Fiber.return () in
  if Memo.Invalidation.is_empty invalidation
  then Fiber.return ()
  else (
    let now = Time.now () in
    if Option.is_none t.watch_restart_started_at
    then t.watch_restart_started_at <- Some now;
    add_pending_reset t invalidation;
    t.input_change_generation <- t.input_change_generation + 1;
    let* () =
      match t.status with
      | Restarting_build _ -> Fiber.return ()
      | Standing_by ->
        clear_status_overlay t;
        Fiber.return ()
      | Building run_id ->
        set_status_overlay
          t
          (Live
             (fun () ->
               let elapsed = Time.diff (Time.now ()) now |> Time.Span.to_secs in
               Pp.textf "Restarting current build... (%.1fs)" elapsed));
        t.status <- Restarting_build run_id;
        let+ () = Process.Build.cancel_current () in
        Dune_trace.emit Build (fun () ->
          Dune_trace.Event.watch_build_restart
            ~run_id:(Run_id.to_int run_id)
            ~reasons:(Memo.Invalidation.details_hum ~max_elements:max_int invalidation)
            ~at:now)
    in
    trigger_wakeup t)
;;

let rec handle_file_events t file_watcher =
  File_watcher.read file_watcher
  >>= function
  | None ->
    Scheduler.shutdown `Ok;
    Fiber.return ()
  | Some events ->
    let* () = request_restart t (invalidation_of_file_events events) in
    handle_file_events t file_watcher
;;

let create () =
  { status = Standing_by
  ; pending_reset = None
  ; watch_restart_started_at = None
  ; status_overlay = None
  ; wakeup = Trigger.create ()
  ; wakeup_generation = 0
  ; input_change_generation = 0
  ; next_run_id = 1
  ; rpc_requests = Rpc_request_id.Map.empty
  ; state = `Awaiting_init
  }
;;

let run t f =
  let* () = Fiber.return () in
  (match t.state with
   | `Awaiting_init -> t.state <- `Init
   | `Init -> Code_error.raise "may not run a build loop more than once" []);
  match Scheduler.file_watcher () with
  | None ->
    ignore (Fs_memo.init ~dune_file_watcher:None : Memo.Invalidation.t);
    Fiber.finalize f ~finally:(fun () ->
      clear_status_overlay t;
      Fiber.return ())
  | Some file_watcher ->
    Fs_memo.init ~dune_file_watcher:(Some file_watcher) |> Memo.reset;
    Fiber.fork_and_join_unit
      (fun () -> handle_file_events t file_watcher)
      (fun () ->
         Fiber.finalize f ~finally:(fun () ->
           clear_status_overlay t;
           File_watcher.close file_watcher;
           Fiber.return ()))
;;

let reset_wakeup t =
  let previous_wakeup = t.wakeup in
  t.wakeup <- Trigger.create ();
  (* Wake waiters on the previous trigger generation. Replacing the trigger lets
     waiters observe only changes that happen after their current wait point. *)
  Trigger.trigger previous_wakeup
;;

let run_current_build
      ({ watch_restart_started_at = restart_started_at
       ; input_change_generation = build_start_input_change_generation
       ; wakeup_generation = build_start_wakeup_generation
       ; _
       } as t)
      ~action_runner
      ~run_id
      request
  =
  let* () = reset_wakeup t in
  (match t.status with
   | Building _ -> assert false
   | Standing_by | Restarting_build _ -> ());
  t.status <- Building run_id;
  clear_status_overlay t;
  let* outcome =
    let build_ctx =
      Process.Build.create ~action_runner ~run_id ~cancellation:(Fiber.Cancel.create ())
    in
    Fiber.finalize
      (fun () ->
         Process.Build.with_ build_ctx (fun () ->
           Build_system.run_build_requests ?restart_started_at ~build:build_ctx request))
      ~finally:(fun () ->
        match action_runner with
        | None -> Fiber.return ()
        | Some action_runner ->
          Action_runner.complete_build
            action_runner
            ~run_id
            ~cancellation:(Process.Build.cancellation build_ctx))
  in
  let+ () = Scheduler.cleanup_subreaper_child_processes () in
  let next =
    match t.status with
    | Restarting_build _ -> `Restart
    | Standing_by | Building _ ->
      t.status <- Standing_by;
      t.watch_restart_started_at <- None;
      `Done
  in
  outcome, next, build_start_input_change_generation, build_start_wakeup_generation
;;

let rec wait_for_wakeup_after t wakeup_generation =
  let* () = Fiber.return () in
  if t.wakeup_generation <> wakeup_generation
  then Fiber.return ()
  else (
    let wakeup = t.wakeup in
    let* () = Trigger.wait wakeup in
    wait_for_wakeup_after t wakeup_generation)
;;

let cancel_rpc_requests t ~f =
  let* () = Fiber.return () in
  match
    Rpc_request_id.Map.to_list t.rpc_requests
    |> List.filter ~f:(fun (id, request) -> f id request)
  with
  | [] -> Fiber.return ()
  | to_cancel ->
    List.iter to_cancel ~f:(fun (id, _) ->
      t.rpc_requests <- Rpc_request_id.Map.remove t.rpc_requests id);
    let* () =
      Fiber.parallel_iter to_cancel ~f:(fun (_, request) ->
        Build_system.Request.Goal.complete request Failure)
    in
    request_rebuild_due_to_rpc_request t
;;

let cancel_rpc_requests_by_session t ~session_id =
  cancel_rpc_requests t ~f:(fun id _ ->
    Rpc.Server.Session.Id.equal (Rpc_request_id.session_id id) session_id)
;;

let cancel_all_rpc_requests t = cancel_rpc_requests t ~f:(fun _ _ -> true)

let submit_rpc_request t ~session_id ~request_id ~build =
  let request = Build_system.Request.Goal.create build in
  let id = Rpc_request_id.create ~session_id ~request_id in
  let* () = Fiber.return () in
  (match Rpc_request_id.Map.find t.rpc_requests id with
   | Some _ ->
     Code_error.raise
       "RPC build request with this id is already active"
       [ "id", Rpc_request_id.to_dyn id ]
   | None -> t.rpc_requests <- Rpc_request_id.Map.add_exn t.rpc_requests id request);
  let* () = request_rebuild_due_to_rpc_request t in
  Build_system.Request.Goal.await request
;;

type rpc_poll_iter_result =
  { wakeup_generation : int
  ; sticky_built_at : int option
  }

let rpc_poll_iter t ~action_runner ~sticky_goal ~sticky_built_at =
  let rec loop () =
    let rpc_requests = Rpc_request_id.Map.to_list t.rpc_requests in
    let sticky_goal_to_build =
      match sticky_goal with
      | None -> None
      | Some sticky_goal ->
        let has_rpc_requests = not (List.is_empty rpc_requests) in
        (match sticky_built_at with
         | Some generation
           when (not has_rpc_requests) && Int.equal generation t.input_change_generation
           -> None
         | None | Some _ -> Some sticky_goal)
    in
    match sticky_goal_to_build, rpc_requests with
    | None, [] ->
      (match t.status with
       | Restarting_build _ ->
         t.status <- Standing_by;
         t.watch_restart_started_at <- None
       | Standing_by | Building _ -> ());
      let* () = reset_wakeup t in
      Fiber.return { wakeup_generation = t.wakeup_generation; sticky_built_at = None }
    | _ ->
      let* res, next, input_change_generation, wakeup_generation =
        let rpc_requests_in_build = List.map rpc_requests ~f:snd in
        let request =
          let goals =
            match sticky_goal_to_build with
            | None -> rpc_requests_in_build
            | Some sticky_goal ->
              Build_system.Request.Goal.create sticky_goal :: rpc_requests_in_build
          in
          Build_system.Request.create goals
        in
        let run_id = next_watch_run_id t in
        let () =
          match t.pending_reset with
          | None -> Memo.Metrics.reset ()
          | Some invalidation ->
            t.pending_reset <- None;
            if not (Memo.Invalidation.is_empty invalidation)
            then (
              let details_hum = Memo.Invalidation.details_hum invalidation in
              Console.maybe_clear_screen ~details_hum);
            Memo.reset invalidation
        in
        run_current_build t ~action_runner ~run_id request
      in
      (match next with
       | `Restart -> loop ()
       | `Done ->
         let+ () =
           let outcome =
             match res with
             | Ok () -> Build_outcome.Success
             | Error `Already_reported -> Failure
           in
           let+ () =
             Fiber.sequential_iter rpc_requests ~f:(fun (id, _) ->
               match Rpc_request_id.Map.find t.rpc_requests id with
               | None -> Fiber.return ()
               | Some request ->
                 t.rpc_requests <- Rpc_request_id.Map.remove t.rpc_requests id;
                 Build_system.Request.Goal.complete request outcome)
           in
           build_finish t outcome
         in
         { wakeup_generation
         ; sticky_built_at =
             (if Option.is_some sticky_goal_to_build
              then Some input_change_generation
              else None)
         })
  in
  loop ()
;;

let poll t ~action_runner ~sticky_goal =
  let rec loop ~sticky_built_at =
    let* { wakeup_generation; sticky_built_at = built } =
      rpc_poll_iter t ~action_runner ~sticky_goal ~sticky_built_at
    in
    (* Work we're allowed to do between successive polling iterations. this work
   should be fast and never fail (within reason) *)
    Dune_trace.emit ~buffered:true Scheduler Dune_trace.Event.scheduler_idle;
    Dune_trace.flush ();
    let sticky_built_at = Option.first_some built sticky_built_at in
    let* () = wait_for_wakeup_after t wakeup_generation in
    loop ~sticky_built_at
  in
  loop ~sticky_built_at:None
;;
