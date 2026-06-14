open Import
open Fiber.O
module Server = Root.Rpc.Server
module Request = Action_runner_protocol.Request
module Decl = Action_runner_protocol.Decl

type session = Session : _ Server.Session.t -> session

type initialized =
  { generation : int
  ; session : session
  }

type starting =
  { generation : int
  ; ready : unit Fiber.Ivar.t
  }

type status =
  | Not_started
  | Starting of starting
  | Initialized of initialized
  | Closed

module Request_id = Stdune.Id.Make ()

type t =
  { name : Action_runner_name.t
  ; mutable status : status
  ; start_lock : Fiber.Mutex.t
  ; mutable next_generation : int
  ; mutable start : (runner:t -> generation:int -> unit Fiber.t) option
  }

let name t = t.name

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

let generation_matches generation current =
  match generation with
  | None -> true
  | Some generation -> Int.equal generation current
;;

let disconnect ?generation t =
  let ready =
    match t.status with
    | Not_started | Closed -> None
    | Starting { generation = current; ready } when generation_matches generation current
      -> Some (Some ready)
    | Initialized { generation = current; _ } when generation_matches generation current
      -> Some None
    | Starting _ | Initialized _ -> None
  in
  match ready with
  | None -> Fiber.return ()
  | Some ready ->
    t.status <- Closed;
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.runner_disconnected ~name:t.name);
    (match ready with
     | None -> Fiber.return ()
     | Some ready -> Fiber.Ivar.fill ready ())
;;

let await_initialized t =
  match t.status with
  | Not_started | Closed -> disconnected t
  | Initialized s -> Fiber.return s
  | Starting { generation; ready } ->
    let+ () = Fiber.Ivar.read ready in
    (match t.status with
     | Closed -> disconnected_before_initialization t
     | Initialized ({ generation = current; _ } as s) when Int.equal generation current ->
       s
     | Not_started | Starting _ | Initialized _ ->
       Code_error.raise
         "action runner initialization finished without the expected session"
         [ "name", Dyn.string (Action_runner_name.to_string t.name)
         ; "generation", Dyn.int generation
         ])
;;

let await_ready t =
  let* _initialized = await_initialized t in
  Fiber.return ()
;;

let set_start t start =
  match t.start with
  | None -> t.start <- Some start
  | Some _ -> Code_error.raise "action runner start has already been set" []
;;

let ensure_started t =
  Fiber.Mutex.with_lock t.start_lock ~f:(fun () ->
    match t.status with
    | Initialized _ | Starting _ -> await_ready t
    | Not_started | Closed ->
      let generation = t.next_generation in
      t.next_generation <- generation + 1;
      let ready = Fiber.Ivar.create () in
      t.status <- Starting { generation; ready };
      let* result =
        Fiber.collect_errors (fun () ->
          match t.start with
          | Some start -> start ~runner:t ~generation
          | None -> Code_error.raise "action runner start has not been set" [])
      in
      (match result with
       | Ok () -> await_ready t
       | Error exns ->
         let* () = disconnect t ~generation in
         Fiber.reraise_all exns))
;;

let send_request ~request ~payload t =
  let* { generation; session } = await_initialized t in
  let (Session session) = session in
  let id =
    Dune_rpc.Id.make (Sexp.Atom (Int.to_string (Request_id.to_int (Request_id.gen ()))))
  in
  let* result =
    Fiber.collect_errors (fun () ->
      Server.Session.request session (Dune_rpc.Decl.Request.witness request) id payload)
  in
  let is_connection_dead { Exn_with_backtrace.exn; _ } =
    match exn with
    | Dune_rpc.Response.Error.E { kind = Connection_dead; _ } -> true
    | _ -> false
  in
  match result with
  | Ok response -> Fiber.return response
  | Error exns when List.exists exns ~f:is_connection_dead ->
    let* () = disconnect t ~generation in
    disconnected t
  | Error exns -> Fiber.reraise_all exns
;;

let cancel_build t ~run_id =
  let payload = { Request.Cancel_build.run_id } in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.runner_cancel_request_sent ~name:t.name);
  send_request ~request:Decl.cancel_build ~payload t
;;

let exec_process_uncancelled t ~run_id process =
  let* () = ensure_started t in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.runner_request_sent ~name:t.name);
  let payload = { Request.Exec.run_id; process } in
  let+ (response : Request.Exec.response) = send_request ~request:Decl.exec ~payload t in
  match response with
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
    { workers : (string, t) Table.t
    ; pool : Fiber.Pool.t
    }

  let create () =
    { workers = Table.create (module String) 16; pool = Fiber.Pool.create () }
  ;;

  let run t = Fiber.Pool.run t.pool
  let stop t = Fiber.Pool.close t.pool

  let register t worker =
    match Table.add t.workers (Action_runner_name.to_string worker.name) worker with
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
    @@ fun session ({ Request.Ready.name; generation } : Request.Ready.t) ->
    match Table.find t.workers (Action_runner_name.to_string name) with
    | None -> invalid_request "unexpected action runner"
    | Some worker ->
      (match worker.status with
       | Not_started | Closed -> invalid_request "disconnected earlier"
       | Initialized _ -> invalid_request "already signalled readiness to the server"
       | Starting { generation = expected; ready } ->
         if not (Int.equal generation expected)
         then invalid_request "unexpected action runner generation";
         let initialized = { generation; session = Session session } in
         worker.status <- Initialized initialized;
         Dune_trace.emit Action (fun () ->
           Dune_trace.Event.Action.runner_connected ~name:worker.name);
         let* () =
           Fiber.Pool.task t.pool ~f:(fun () ->
             let* () = Server.Session.closed session in
             disconnect worker ~generation)
         in
         Fiber.Ivar.fill ready ())
  ;;
end

let create server ~name =
  let t =
    { name
    ; status = Not_started
    ; start_lock = Fiber.Mutex.create ()
    ; next_generation = 0
    ; start = None
    }
  in
  Rpc_server.register server t;
  t
;;
