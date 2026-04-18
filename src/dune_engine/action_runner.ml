open Import
open Fiber.O
module Server = Root.Rpc.Server
module Request = Action_runner_protocol.Request
module Decl = Action_runner_protocol.Decl

type session = Session : _ Server.Session.t -> session

module Worker = struct
  type t =
    { pid : Pid.t
    ; is_process_group_leader : bool
    }

  let terminate t =
    let pid = Pid.to_int t.pid in
    let pid = if Sys.win32 || not t.is_process_group_leader then pid else -pid in
    match Unix.kill pid Sys.sigterm with
    | () -> ()
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  ;;
end

type initialized = { session : session }
type starting = { ready : unit Fiber.Ivar.t }

type status =
  | Starting of starting
  | Initialized of initialized
  | Closed

module Request_id = Stdune.Id.Make ()

type t =
  { name : Action_runner_name.t
  ; mutable status : status
  ; worker : Worker.t
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
  let ready =
    match t.status with
    | Closed -> None
    | Starting { ready } -> Some (Some ready)
    | Initialized _ -> Some None
  in
  match ready with
  | None -> Fiber.return ()
  | Some ready ->
    t.status <- Closed;
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.Runner.runner_event
        ~name:t.name
        Dune_trace.Event.Action.Runner.Disconnected);
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
        Scheduler.wait_for_process
          t.worker.pid
          ~is_process_group_leader:t.worker.is_process_group_leader
      in
      disconnect t))
;;

let ensure_ready t =
  let* () = monitor_worker t in
  await_ready t
;;

let send_request ~request ~payload t =
  let* { session = Session session } = await_initialized t in
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
    Dune_trace.Event.Action.Runner.runner_event
      ~name:t.name
      Dune_trace.Event.Action.Runner.Cancel_request_sent);
  send_request ~request:Decl.cancel_build ~payload t
;;

let exec_process_uncancelled t ~run_id process =
  let* () = ensure_ready t in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event
      ~name:t.name
      Dune_trace.Event.Action.Runner.Request_sent);
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
    Fiber.Cancel.with_handler
      cancellation
      (fun () ->
         Fiber.collect_errors (fun () -> exec_process_uncancelled t ~run_id process))
      ~on_cancel
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
    Table.iter t.workers ~f:(fun worker -> Worker.terminate worker.worker);
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

  let implement_handler t (handler : _ Root.Rpc.Server.Handler.t) =
    Server.Handler.declare_request handler Decl.exec;
    Server.Handler.declare_request handler Decl.cancel_build;
    Server.Handler.implement_request handler Decl.ready
    @@ fun session ({ Request.Ready.name } : Request.Ready.t) ->
    match Table.find t.workers name with
    | None -> invalid_request "unexpected action runner"
    | Some worker ->
      (match worker.status with
       | Closed -> invalid_request "disconnected earlier"
       | Initialized _ -> invalid_request "already signalled readiness to the server"
       | Starting { ready } ->
         worker.status <- Initialized { session = Session session };
         Dune_trace.emit Action (fun () ->
           Dune_trace.Event.Action.Runner.runner_event
             ~name:worker.name
             Dune_trace.Event.Action.Runner.Connected);
         let* () =
           Fiber.Pool.task t.pool ~f:(fun () ->
             let* () = Server.Session.closed session in
             disconnect worker)
         in
         Fiber.Ivar.fill ready ())
  ;;
end

let create server ~name ~(worker : Worker.t) =
  let { Rpc_server.pool = monitor_pool; _ } = server in
  let t =
    { name
    ; status = Starting { ready = Fiber.Ivar.create () }
    ; worker
    ; monitor_pool
    ; monitoring = false
    }
  in
  Rpc_server.register server t;
  t
;;
