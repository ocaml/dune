open Import
open Fiber.O
module Client = Root.Rpc.Client
module Server = Root.Rpc.Server

module Name = struct
  type t = string

  module M = Dune_util.Stringlike.Make (struct
      type nonrec t = t

      let module_ = "Action_runner.Name"
      let description = "action runner name"

      let description_of_valid_string =
        Some
          (Pp.text
             "Action runner names must be non-empty, may not contain '/', and may not be \
              '.'.")
      ;;

      let hint_valid = None

      let of_string_opt s =
        Option.some_if
          ((not (String.is_empty s))
           && (not (String.equal s "."))
           && not (String.exists s ~f:(Char.equal '/')))
          s
      ;;

      let to_string t = t
    end)

  include (M : module type of M with type t := t)

  let repr = Repr.view Repr.string ~to_:to_string
end

module Request = struct
  module Exec = struct
    type t =
      { run_id : Dune_scheduler.Run_id.t
      ; action : Action_exec.input
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
        Decl.Request.make_current_gen ~req:Conv.string ~resp:Conv.unit ~version:1
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
type initialized = { session : session }

type status =
  | Awaiting_initialization of unit Fiber.Ivar.t
  | Initialized of initialized
  | Closed

let status_repr =
  Repr.variant
    "action-runner-status"
    [ Repr.case0 "Awaiting_initialization" ~test:(function
        | Awaiting_initialization _ -> true
        | Initialized _ | Closed -> false)
    ; Repr.case0 "Initialized" ~test:(function
        | Initialized _ -> true
        | Awaiting_initialization _ | Closed -> false)
    ; Repr.case0 "Closed" ~test:(function
        | Closed -> true
        | Awaiting_initialization _ | Initialized _ -> false)
    ]
;;

module Id = Stdune.Id.Make ()
module Request_id = Stdune.Id.Make ()

type t =
  { name : Name.t
  ; id : Id.t
  ; mutable status : status
  }

let name t = t.name

let await_initialized t =
  match t.status with
  | Closed ->
    Code_error.raise
      "action runner disconnected"
      [ "name", Dyn.string (Name.to_string t.name) ]
  | Initialized s -> Fiber.return s
  | Awaiting_initialization ready ->
    let+ () = Fiber.Ivar.read ready in
    (match t.status with
     | Closed ->
       Code_error.raise
         "action runner disconnected before initialization"
         [ "name", Dyn.string (Name.to_string t.name) ]
     | Initialized s -> s
     | Awaiting_initialization _ ->
       Code_error.raise
         "action runner initialization finished without a session"
         [ "name", Dyn.string (Name.to_string t.name) ])
;;

let await_ready t =
  let* _initialized = await_initialized t in
  Fiber.return ()
;;

let disconnect t =
  match t.status with
  | Closed -> Fiber.return ()
  | Initialized _ ->
    t.status <- Closed;
    Fiber.return ()
  | Awaiting_initialization ready ->
    t.status <- Closed;
    Fiber.Ivar.fill ready ()
;;

let send_request ?trace_request_sent ~request ~payload t =
  let* { session } = await_initialized t in
  let (Session session) = session in
  (match trace_request_sent with
   | None -> ()
   | Some name ->
     Dune_trace.emit Action (fun () -> Dune_trace.Event.Action.runner_request_sent ~name));
  let id =
    Dune_rpc.Id.make (Sexp.Atom (Int.to_string (Request_id.to_int (Request_id.gen ()))))
  in
  Server.Session.request session (Dune_rpc.Decl.Request.witness request) id payload
;;

let exec_action t ~run_id action =
  let payload = { Request.Exec.run_id; action } in
  send_request ~trace_request_sent:(Name.to_string t.name) ~request:Decl.exec ~payload t
;;

let cancel_build t ~run_id =
  (* This is currently best-effort only. A running worker action may still
     fail to unwind promptly under [--stop-on-first-error]. *)
  let payload = { Request.Cancel_build.run_id } in
  let+ _result =
    Fiber.collect_errors (fun () -> send_request ~request:Decl.cancel_build ~payload t)
  in
  ()
;;

let _repr =
  let id_repr = Repr.abstract Id.to_dyn in
  Repr.record
    "action-runner"
    [ Repr.field "name" Name.repr ~get:(fun t -> t.name)
    ; Repr.field "id" id_repr ~get:(fun t -> t.id)
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
    match Table.add t.workers (Name.to_string worker.name) worker with
    | Ok () -> ()
    | Error _ ->
      User_error.raise
        [ Pp.textf "Cannot register %s as it already exists" (Name.to_string worker.name)
        ]
  ;;

  let implement_handler t (handler : _ Root.Rpc.Server.Handler.t) =
    Server.Handler.declare_request handler Decl.exec;
    Server.Handler.declare_request handler Decl.cancel_build;
    Server.Handler.implement_request handler Decl.ready
    @@ fun session name ->
    match Table.find t.workers name with
    | None ->
      let error =
        Dune_rpc.Response.Error.create
          ~kind:Invalid_request
          ~message:"unexpected action runner"
          ()
      in
      raise (Dune_rpc.Response.Error.E error)
    | Some worker ->
      (match worker.status with
       | Closed ->
         let error =
           Dune_rpc.Response.Error.create
             ~kind:Invalid_request
             ~message:"disconnected earlier"
             ()
         in
         raise (Dune_rpc.Response.Error.E error)
       | Initialized _ ->
         let error =
           Dune_rpc.Response.Error.create
             ~kind:Invalid_request
             ~message:"already signalled readiness to the server"
             ()
         in
         raise (Dune_rpc.Response.Error.E error)
       | Awaiting_initialization ivar ->
         let initialized = { session = Session session } in
         worker.status <- Initialized initialized;
         Dune_trace.emit Action (fun () ->
           Dune_trace.Event.Action.runner_connected ~name:(Name.to_string worker.name));
         let* () =
           Fiber.Pool.task t.pool ~f:(fun () ->
             let* () = Server.Session.closed session in
             disconnect worker)
         in
         Fiber.Ivar.fill ivar ())
  ;;
end

let create server ~name =
  let init = Fiber.Ivar.create () in
  let t = { name; id = Id.gen (); status = Awaiting_initialization init } in
  Rpc_server.register server t;
  t
;;

module Worker = struct
  let exec_action ({ Request.Exec.run_id = _; action } : Request.Exec.t) =
    let build_deps _ = Code_error.raise "no dynamic actions yet" [] in
    Action_exec.exec action ~build_deps
  ;;

  let cancel_build ({ Request.Cancel_build.run_id = _ } : Request.Cancel_build.t) =
    Scheduler.cancel_current_build ()
  ;;

  let start ~name ~where =
    let* connection = Client.Connection.connect_exn where in
    let private_menu : Client.proc list =
      [ Client.Request Decl.ready
      ; Client.Handle_request (Decl.exec, exec_action)
      ; Client.Handle_request (Decl.cancel_build, cancel_build)
      ]
    in
    let id = Dune_rpc.Id.make (Sexp.Atom (Name.to_string name)) in
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
        let* response = Client.request client request (Name.to_string name) in
        (match response with
         | Ok () -> Client.disconnected client
         | Error error ->
           User_error.raise
             [ Pp.text "Failed to signal readiness to the server"
             ; Pp.text (Dune_rpc.Response.Error.message error)
             ]))
  ;;
end
