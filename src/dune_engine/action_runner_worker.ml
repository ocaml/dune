open Import
open Fiber.O
module Client = Root.Rpc.Client
module Request = Action_runner_protocol.Request
module Decl = Action_runner_protocol.Decl

type active_build =
  { build : Process.Build.t
  ; mutable count : int
  ; drained : unit Fiber.Ivar.t
  }

type t =
  { active_builds : (Run_id.t, active_build) Table.t
  ; mutable highest_cancelled_run_id : Run_id.t option
  }

let create () =
  { active_builds = Table.create (module Run_id) 8; highest_cancelled_run_id = None }
;;

let mark_cancelled t run_id =
  match t.highest_cancelled_run_id with
  | None -> t.highest_cancelled_run_id <- Some run_id
  | Some highest when Run_id.compare highest run_id = Lt ->
    t.highest_cancelled_run_id <- Some run_id
  | Some _ -> ()
;;

let is_cancelled t run_id =
  match t.highest_cancelled_run_id with
  | None -> false
  | Some highest -> Run_id.compare run_id highest <> Gt
;;

let start_build t run_id =
  match Table.find t.active_builds run_id with
  | Some active ->
    active.count <- active.count + 1;
    active.build
  | None ->
    let build =
      Process.Build.create
        ~action_runner:None
        ~run_id
        ~cancellation:(Fiber.Cancel.create ())
    in
    let active = { build; count = 1; drained = Fiber.Ivar.create () } in
    Table.set t.active_builds run_id active;
    build
;;

let finish_exec t run_id =
  match Table.find t.active_builds run_id with
  | None ->
    Code_error.raise
      "action runner finished an exec request that was not recorded as active"
      [ "run_id", Run_id.to_dyn run_id ]
  | Some active when active.count > 1 ->
    active.count <- active.count - 1;
    Fiber.return ()
  | Some active ->
    Table.remove t.active_builds run_id;
    Fiber.Ivar.fill active.drained ()
;;

let exec_process t ({ Request.Exec.run_id; process } : Request.Exec.t) =
  if is_cancelled t run_id
  then Fiber.return Request.Exec.Cancelled
  else (
    let build = start_build t run_id in
    Fiber.finalize
      ~finally:(fun () -> finish_exec t run_id)
      (fun () ->
         let+ response = Process.exec_locally ~build process in
         Request.Exec.Completed response))
;;

let cancel_build t ~name ({ Request.Cancel_build.run_id } : Request.Cancel_build.t) =
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name Cancel_start);
  mark_cancelled t run_id;
  let active_builds =
    Table.values t.active_builds
    |> List.filter ~f:(fun active ->
      Run_id.compare (Process.Build.run_id active.build) run_id <> Gt)
  in
  let* () =
    Fiber.parallel_iter active_builds ~f:(fun active ->
      let* () = Fiber.Cancel.fire (Process.Build.cancellation active.build) in
      Fiber.Ivar.read active.drained)
  in
  Scheduler.cleanup_subreaper_child_processes ()
;;

let finish_build t ({ Request.Finish_build.run_id } : Request.Finish_build.t) =
  let* () =
    match
      Table.find t.active_builds run_id |> Option.map ~f:(fun active -> active.drained)
    with
    | None -> Fiber.return ()
    | Some drained -> Fiber.Ivar.read drained
  in
  Scheduler.cleanup_subreaper_child_processes ()
;;

let start ~name ~rpc_fd =
  let t = create () in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name Connection_start);
  let connection = Client.Connection.of_fd rpc_fd in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event ~name Connection_established);
  let private_menu : Client.proc list =
    [ Client.Request Decl.ready
    ; Handle_request (Decl.exec, exec_process t)
    ; Handle_request (Decl.cancel_build, cancel_build t ~name)
    ; Handle_request (Decl.finish_build, finish_build t)
    ]
  in
  let initialize =
    let name_string = Action_runner_name.to_string name in
    let id = Dune_rpc.Id.make (Sexp.Atom name_string) in
    Dune_rpc.Initialize.Request.create ~id
  in
  Client.client ~private_menu connection initialize ~f:(fun client ->
    Client.Versioned.prepare_request client (Dune_rpc.Decl.Request.witness Decl.ready)
    >>= function
    | Error version_error ->
      User_error.raise
        [ Pp.textf
            "Server does not agree on the menu. Are you running the same dune binary for \
             the worker?"
        ; Pp.text (Dune_rpc.Version_error.message version_error)
        ]
    | Ok request ->
      Client.request client request { Request.Ready.name }
      >>= (function
       | Ok () -> Client.disconnected client
       | Error error ->
         User_error.raise
           [ Pp.text "Failed to signal readiness to the server"
           ; Pp.text (Dune_rpc.Response.Error.message error)
           ]))
;;
