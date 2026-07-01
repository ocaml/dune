open Import
open Fiber.O
module Server = Root.Rpc.Server
module Request = Action_runner_protocol.Request
module Decl = Action_runner_protocol.Decl

type initialized = Session : _ Server.Session.t -> initialized

type status =
  | Starting of { ready : unit Fiber.Ivar.t }
  | Initialized of initialized
  | Closed

module Request_id = Stdune.Id.Make ()

type t =
  { name : Action_runner_name.t
  ; mutable status : status
  ; pid : Pid.t
  ; pool : Fiber.Pool.t
  }

let disconnected t =
  User_error.raise
    [ Pp.textf "Action runner %S disconnected." (Action_runner_name.to_string t.name) ]
;;

let disconnect t =
  match
    match t.status with
    | Closed -> None
    | Starting { ready } -> Some (Some ready)
    | Initialized _ -> Some None
  with
  | None -> Fiber.return ()
  | Some ready ->
    t.status <- Closed;
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.Runner.runner_event ~name:t.name Disconnected);
    (match ready with
     | None -> Fiber.return ()
     | Some ready -> Fiber.Ivar.fill ready ())
;;

let await_initialized t =
  match t.status with
  | Closed -> disconnected t
  | Initialized s -> Fiber.return s
  | Starting { ready } ->
    let+ () = Fiber.Ivar.read ready in
    (match t.status with
     | Initialized s -> s
     | Closed ->
       User_error.raise
         [ Pp.textf
             "Action runner %S failed to initialize."
             (Action_runner_name.to_string t.name)
         ; Pp.text "It exited before connecting back to Dune."
         ]
     | Starting _ ->
       Code_error.raise
         "action runner initialization finished without a session"
         [ "name", Dyn.string (Action_runner_name.to_string t.name) ])
;;

let ensure_ready t =
  let+ (_initialized : initialized) = await_initialized t in
  ()
;;

let send_request ~request ~payload t =
  let* (Session session) = await_initialized t in
  Fiber.collect_errors (fun () ->
    let id =
      Dune_rpc.Id.make (Sexp.Atom (Int.to_string (Request_id.to_int (Request_id.gen ()))))
    in
    Server.Session.request session (Dune_rpc.Decl.Request.witness request) id payload)
  >>= function
  | Ok response -> Fiber.return response
  | Error exns
    when List.exists exns ~f:(fun { Exn_with_backtrace.exn; _ } ->
           match exn with
           | Dune_rpc.Response.Error.E { kind = Connection_dead; _ } -> true
           | _ -> false) ->
    let* () = disconnect t in
    disconnected t
  | Error exns -> Fiber.reraise_all exns
;;

let cancel_build t ~run_id =
  let payload = { Request.Cancel_build.run_id } in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name:t.name Cancel_request_sent);
  send_request ~request:Decl.cancel_build ~payload t
;;

let finish_build t ~run_id =
  let* () = ensure_ready t in
  let payload = { Request.Finish_build.run_id } in
  send_request ~request:Decl.finish_build ~payload t
;;

let complete_build t ~run_id ~cancellation =
  if Fiber.Cancel.fired cancellation
  then cancel_build t ~run_id
  else finish_build t ~run_id
;;

let exec_process_uncancelled t ~run_id process =
  let* () = ensure_ready t in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name:t.name Request_sent);
  (let payload = { Request.Exec.run_id; process } in
   send_request ~request:Decl.exec ~payload t)
  >>| function
  | Cancelled -> raise (Memo.Non_reproducible Scheduler.Run.Build_cancelled)
  | Completed response ->
    let trace_args =
      [ "action_runner", Sexp.Atom (Action_runner_name.to_string t.name)
      ; "action_runner_pid", Sexp.Atom (Int.to_string (Pid.to_int t.pid))
      ]
      @ response.trace_args
    in
    { response with trace_args }
;;

let exec_process t ~run_id ~cancellation process =
  let on_cancel () = cancel_build t ~run_id in
  let* result, outcome =
    Fiber.Cancel.with_handler ~on_cancel cancellation (fun () ->
      Fiber.collect_errors (fun () -> exec_process_uncancelled t ~run_id process))
  in
  match outcome, result with
  | Cancelled (), _ ->
    raise (Memo.Non_reproducible Dune_scheduler.Scheduler.Run.Build_cancelled)
  | Not_cancelled, Ok result -> Fiber.return result
  | Not_cancelled, Error exns -> Fiber.reraise_all exns
;;

let run t =
  Fiber.fork_and_join_unit
    (fun () -> Fiber.Pool.run t.pool)
    (fun () ->
       let* (_status : Proc.Process_info.t) =
         Scheduler.wait_for_process t.pid ~is_process_group_leader:false
       in
       disconnect t)
;;

let stop t =
  let* () =
    match t.status with
    | Starting _ | Closed -> Fiber.return ()
    | Initialized (Session session) -> Server.Session.close session
  in
  Fiber.Pool.close t.pool
;;

let invalid_request message =
  let error = Dune_rpc.Response.Error.create ~kind:Invalid_request ~message () in
  raise (Dune_rpc.Response.Error.E error)
;;

let ready t session ({ Request.Ready.name } : Request.Ready.t) =
  match t with
  | None -> invalid_request "unexpected action runner"
  | Some worker when not (Action_runner_name.equal name worker.name) ->
    invalid_request "unexpected action runner"
  | Some worker ->
    (match worker.status with
     | Closed -> invalid_request "disconnected earlier"
     | Initialized _ -> invalid_request "already signalled readiness to the server"
     | Starting { ready } ->
       worker.status <- Initialized (Session session);
       Dune_trace.emit Action (fun () ->
         Dune_trace.Event.Action.Runner.runner_event ~name:worker.name Connected);
       let* () =
         Fiber.Pool.task worker.pool ~f:(fun () ->
           let* () = Server.Session.closed session in
           disconnect worker)
       in
       (* An action runner may only connect once, so there's no need to wait
          for tasks here *)
       let* () = Fiber.Pool.close worker.pool in
       Fiber.Ivar.fill ready ())
;;

let implement_handler t (handler : _ Root.Rpc.Server.Handler.t) =
  Server.Handler.declare_request handler Decl.exec;
  Server.Handler.declare_request handler Decl.cancel_build;
  Server.Handler.declare_request handler Decl.finish_build;
  Server.Handler.implement_request handler Decl.ready (ready t)
;;

let create name pid =
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name (Spawn pid));
  Scheduler.preserve_child_process pid;
  { name
  ; status = Starting { ready = Fiber.Ivar.create () }
  ; pid
  ; pool = Fiber.Pool.create ()
  }
;;
