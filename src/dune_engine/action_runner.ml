open Import
open Fiber.O
module Client = Root.Rpc.Client
module Server = Root.Rpc.Server

module Request = struct
  module Exec = struct
    type response =
      | Completed of Process.Runner.response
      | Cancelled

    type t =
      { run_id : Dune_scheduler.Run_id.t
      ; process : Process.Runner.request
      }
  end

  module Ready = struct
    type t =
      { name : Action_runner_name.t
      ; generation : int
      }
  end

  module Cancel_build = struct
    type t = { run_id : Dune_scheduler.Run_id.t }
  end
end

module Decl = struct
  module Conv = Dune_rpc.Conv
  module Decl = Dune_rpc.Decl

  let marshal () =
    let to_ data = Marshal.from_string data in
    let from value = Marshal.to_string value ~sharing:true in
    Conv.iso Conv.string to_ from
  ;;

  module Exec = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:(marshal ()) ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/exec")
        ~generations:[ v1 ]
    ;;
  end

  module Ready = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:Conv.unit ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/ready")
        ~generations:[ v1 ]
    ;;
  end

  module Cancel_build = struct
    let decl =
      let v1 =
        Decl.Request.make_current_gen ~req:(marshal ()) ~resp:Conv.unit ~version:1
      in
      Decl.Request.make
        ~method_:(Dune_rpc.Method.Name.of_string "action/cancel-build")
        ~generations:[ v1 ]
    ;;
  end

  let exec = Exec.decl
  let ready = Ready.decl
  let cancel_build = Cancel_build.decl
end

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

let status_repr =
  Repr.variant
    "action-runner-status"
    [ Repr.case0 "Not_started" ~test:(function
        | Not_started -> true
        | Starting _ | Initialized _ | Closed -> false)
    ; Repr.case0 "Starting" ~test:(function
        | Starting _ -> true
        | Not_started | Initialized _ | Closed -> false)
    ; Repr.case0 "Initialized" ~test:(function
        | Initialized _ -> true
        | Not_started | Starting _ | Closed -> false)
    ; Repr.case0 "Closed" ~test:(function
        | Closed -> true
        | Not_started | Starting _ | Initialized _ -> false)
    ]
;;

module Request_id = Stdune.Id.Make ()

type t =
  { name : Action_runner_name.t
  ; mutable status : status
  ; start_lock : Fiber.Mutex.t
  ; mutable next_generation : int
  ; mutable start : (runner:t -> generation:int -> unit Fiber.t) option
  }

type worker = t

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

let start_exn t =
  match t.start with
  | Some start -> start
  | None -> Code_error.raise "action runner start has not been set" []
;;

let prepare_to_start t =
  let generation = t.next_generation in
  t.next_generation <- generation + 1;
  let ready = Fiber.Ivar.create () in
  t.status <- Starting { generation; ready };
  generation
;;

let ensure_started t =
  Fiber.Mutex.with_lock t.start_lock ~f:(fun () ->
    match t.status with
    | Initialized _ | Starting _ -> await_ready t
    | Not_started | Closed ->
      let generation = prepare_to_start t in
      let* result = Fiber.collect_errors (fun () -> start_exn t ~runner:t ~generation) in
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

let exec_process t ~run_id process =
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

let cancel_build t ~run_id =
  let payload = { Request.Cancel_build.run_id } in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.runner_cancel_request_sent ~name:t.name);
  let* () = send_request ~request:Decl.cancel_build ~payload t in
  Fiber.return ()
;;

let _repr =
  Repr.record
    "action-runner"
    [ Repr.field "name" Action_runner_name.repr ~get:(fun t -> t.name)
    ; Repr.field "status" status_repr ~get:(fun t -> t.status)
    ]
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

module Worker = struct
  type active_build =
    { run_id : Dune_scheduler.Run_id.t
    ; mutable count : int
    ; drained : unit Fiber.Ivar.t
    }

  let active_build : active_build option ref = ref None
  let highest_cancelled_run_id : Dune_scheduler.Run_id.t option ref = ref None
  let run_id_leq a b = Dune_scheduler.Run_id.compare a b <> Gt

  let mark_cancelled run_id =
    match !highest_cancelled_run_id with
    | None -> highest_cancelled_run_id := Some run_id
    | Some highest when Dune_scheduler.Run_id.compare highest run_id = Lt ->
      highest_cancelled_run_id := Some run_id
    | Some _ -> ()
  ;;

  let is_cancelled run_id =
    match !highest_cancelled_run_id with
    | None -> false
    | Some highest -> run_id_leq run_id highest
  ;;

  let start_build run_id =
    match !active_build with
    | None ->
      Scheduler.reset_current_build_cancellation ();
      active_build := Some { run_id; count = 1; drained = Fiber.Ivar.create () }
    | Some active when active.run_id = run_id -> active.count <- active.count + 1
    | Some active ->
      Code_error.raise
        "action runner cannot serve multiple build runs at once"
        [ "active_run_id", Dyn.int (Dune_scheduler.Run_id.to_int active.run_id)
        ; "requested_run_id", Dyn.int (Dune_scheduler.Run_id.to_int run_id)
        ]
  ;;

  let finish_build run_id =
    match !active_build with
    | None ->
      Code_error.raise
        "action runner finished a build that was not recorded as active"
        [ "run_id", Dyn.int (Dune_scheduler.Run_id.to_int run_id) ]
    | Some active when active.run_id = run_id && active.count > 1 ->
      active.count <- active.count - 1;
      Fiber.return ()
    | Some active when active.run_id = run_id && active.count = 1 ->
      active_build := None;
      Fiber.Ivar.fill active.drained ()
    | Some active ->
      Code_error.raise
        "action runner finished a different build than the active one"
        [ "active_run_id", Dyn.int (Dune_scheduler.Run_id.to_int active.run_id)
        ; "finished_run_id", Dyn.int (Dune_scheduler.Run_id.to_int run_id)
        ]
  ;;

  let exec_process ~name ({ Request.Exec.run_id; process } : Request.Exec.t) =
    if is_cancelled run_id
    then Fiber.return Request.Exec.Cancelled
    else (
      start_build run_id;
      Fiber.finalize
        (fun () ->
           Dune_trace.emit Action (fun () ->
             Dune_trace.Event.Action.runner_exec_start ~name);
           let+ response = Process.exec_locally process in
           Request.Exec.Completed response)
        ~finally:(fun () -> finish_build run_id))
  ;;

  let cancel_build ~name ({ Request.Cancel_build.run_id } : Request.Cancel_build.t) =
    Dune_trace.emit Action (fun () -> Dune_trace.Event.Action.runner_cancel_start ~name);
    mark_cancelled run_id;
    match !active_build with
    | Some active when run_id_leq active.run_id run_id ->
      let drained = active.drained in
      let* () = Scheduler.cancel_current_build () in
      Fiber.Ivar.read drained
    | None | Some _ -> Fiber.return ()
  ;;

  let start ~name ~generation ~where =
    let name_string = Action_runner_name.to_string name in
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.runner_connection_start ~name);
    let* connection = Client.Connection.connect_exn where in
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.runner_connection_established ~name);
    let private_menu : Client.proc list =
      [ Client.Request Decl.ready
      ; Client.Handle_request (Decl.exec, exec_process ~name)
      ; Client.Handle_request (Decl.cancel_build, cancel_build ~name)
      ]
    in
    let id = Dune_rpc.Id.make (Sexp.Atom (sprintf "%s:%d" name_string generation)) in
    let initialize = Dune_rpc.Initialize.Request.create ~id in
    Client.client ~private_menu connection initialize ~f:(fun client ->
      let* request =
        Client.Versioned.prepare_request client (Dune_rpc.Decl.Request.witness Decl.ready)
      in
      match request with
      | Error version_error ->
        User_error.raise
          [ Pp.textf
              "Server does not agree on the menu. Are you running the same dune binary \
               for the worker?"
          ; Pp.text (Dune_rpc.Version_error.message version_error)
          ]
      | Ok request ->
        let payload = { Request.Ready.name; generation } in
        let* response = Client.request client request payload in
        (match response with
         | Ok () -> Client.disconnected client
         | Error error ->
           User_error.raise
             [ Pp.text "Failed to signal readiness to the server"
             ; Pp.text (Dune_rpc.Response.Error.message error)
             ]))
  ;;
end
