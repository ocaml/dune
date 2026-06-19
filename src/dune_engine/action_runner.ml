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
  ; monitor_pool : Fiber.Pool.t
  ; mutable monitoring : bool
  }

let disconnected t =
  User_error.raise
    [ Pp.textf "Action runner %S disconnected." (Action_runner_name.to_string t.name) ]
;;

let disconnected_before_initialization t =
  User_error.raise
    [ Pp.textf
        "Action runner %S failed to initialize."
        (Action_runner_name.to_string t.name)
    ; Pp.text "It exited before connecting back to Dune."
    ]
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
     | Closed -> disconnected_before_initialization t
     | Initialized s -> s
     | Starting _ ->
       Code_error.raise
         "action runner initialization finished without a session"
         [ "name", Dyn.string (Action_runner_name.to_string t.name) ])
;;

let await_ready t =
  let+ (_initialized : initialized) = await_initialized t in
  ()
;;

let monitor_worker t =
  if t.monitoring
  then Fiber.return ()
  else (
    t.monitoring <- true;
    Fiber.Pool.task t.monitor_pool ~f:(fun () ->
      let* (_status : Proc.Process_info.t) =
        Scheduler.wait_for_process t.pid ~is_process_group_leader:true
      in
      disconnect t))
;;

let ensure_ready t =
  let* () = monitor_worker t in
  await_ready t
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
      ("action_runner", Sexp.Atom (Action_runner_name.to_string t.name))
      :: response.trace_args
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

module Rpc_server = struct
  type nonrec t =
    { workers : (Action_runner_name.t, t) Table.t
    ; pool : Fiber.Pool.t
    }

  let create () =
    { workers = Table.create (module Action_runner_name) 16; pool = Fiber.Pool.create () }
  ;;

  let run t = Fiber.Pool.run t.pool

  let stop t =
    Table.iter t.workers ~f:(fun worker ->
      let pid = Pid.to_int worker.pid in
      let pid = if Sys.win32 then pid else -pid in
      Unix.kill pid Sys.sigterm);
    Fiber.Pool.close t.pool
  ;;

  let register t worker =
    match Table.add t.workers worker.name worker with
    | Ok () -> ()
    | Error _ ->
      User_error.raise
        [ Pp.textf
            "Cannot register %s as it already exists"
            (Action_runner_name.to_string worker.name)
        ]
  ;;

  let invalid_request message =
    let error = Dune_rpc.Response.Error.create ~kind:Invalid_request ~message () in
    raise (Dune_rpc.Response.Error.E error)
  ;;

  let ready t session ({ Request.Ready.name } : Request.Ready.t) =
    match Table.find t.workers name with
    | None -> invalid_request "unexpected action runner"
    | Some worker ->
      (match worker.status with
       | Closed -> invalid_request "disconnected earlier"
       | Initialized _ -> invalid_request "already signalled readiness to the server"
       | Starting { ready } ->
         worker.status <- Initialized (Session session);
         Dune_trace.emit Action (fun () ->
           Dune_trace.Event.Action.Runner.runner_event ~name:worker.name Connected);
         let* () =
           Fiber.Pool.task t.pool ~f:(fun () ->
             let* () = Server.Session.closed session in
             disconnect worker)
         in
         Fiber.Ivar.fill ready ())
  ;;

  let implement_handler t (handler : _ Root.Rpc.Server.Handler.t) =
    Server.Handler.declare_request handler Decl.exec;
    Server.Handler.declare_request handler Decl.cancel_build;
    Server.Handler.implement_request handler Decl.ready (ready t)
  ;;
end

let create server name pid =
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name (Spawn pid));
  let { Rpc_server.pool = monitor_pool; _ } = server in
  let t =
    { name
    ; status = Starting { ready = Fiber.Ivar.create () }
    ; pid
    ; monitor_pool
    ; monitoring = false
    }
  in
  Rpc_server.register server t;
  t
;;
