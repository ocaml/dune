open Import
open Fiber.O

include struct
  open Dune_rpc
  module Initialize = Initialize
  module Progress = Progress
  module Procedures = Procedures
  module Diagnostic = Diagnostic
  module Job = Job
end

include struct
  open Dune_engine
  module Action_builder = Action_builder
  module Build_loop = Build_loop
  module Diff_promotion = Diff_promotion
  module Action_runner = Action_runner
end

include struct
  open Decl
  module Status = Status
end

module Session = Rpc.Server.Session
module Handler = Rpc.Server.Handler
module Source = Rpc.Server.Source
module Csexp_rpc = Rpc.Csexp_rpc

type build_request =
  | Build of Dune_lang.Dep_conf.t list
  | Runtest of string list

type build =
  | Disabled
  | Enabled of
      { build_loop : Build_loop.t
      ; build_action : build_request -> unit Action_builder.t
      }

module Clients = struct
  type entry = { session : unit Session.Stage1.t }
  type t = entry Session.Id.Map.t

  let empty = Session.Id.Map.empty

  let add_session t (session : _ Session.Stage1.t) =
    let id = Session.Stage1.id session in
    let result = { session } in
    Session.Id.Map.add_exn t id result
  ;;

  let remove_session t (session : _ Session.Stage1.t) =
    let id = Session.Stage1.id session in
    Session.Id.Map.remove t id
  ;;

  let to_list = Session.Id.Map.to_list
  let to_list_map = Session.Id.Map.to_list_map

  let repr =
    let session_repr = Repr.abstract (Session.Stage1.to_dyn Dyn.unit) in
    Repr.view (Repr.list session_repr) ~to_:(fun t ->
      Session.Id.Map.values t |> List.map ~f:(fun { session } -> session))
  ;;
end

type server =
  { lifecycle : Rpc.Server.Lifecycle.t
  ; action_runner : Action_runner.t option
  ; watch_mode : Watch_mode_config.t
  ; mutable clients : Clients.t
  }

let repr =
  Repr.record "rpc" [ Repr.field "connections" Clients.repr ~get:(fun t -> t.clients) ]
;;

let to_dyn = Repr.to_dyn repr
let current : server option ref = ref None

type t =
  { server : server
  ; build : build
  }

let () =
  Debug.register ~name:"rpc" (fun () ->
    match !current with
    | None -> Dyn.Option None
    | Some server -> to_dyn server)
;;

let stop (t : t) =
  Fiber.fork_and_join_unit
    (fun () ->
       match t.server.action_runner with
       | None -> Fiber.return ()
       | Some runner -> Action_runner.stop runner)
    (fun () -> Rpc.Server.Lifecycle.stop t.server.lifecycle)
;;

let current_errors () =
  Fiber.Svar.read Build_system.errors
  |> Build_system_error.Set.current
  |> Build_system_error.Id.Map.values
;;

let diff_map_entry ~on_add ~on_remove last now =
  match last, now with
  | Some last, None -> Some (on_remove last)
  | None, Some now -> Some (on_add now)
  | Some _, Some _ | None, None -> None
;;

let submit_build_request t session request_id kind =
  match t.build with
  | Disabled ->
    Code_error.raise "RPC build request received by a server without build handling" []
  | Enabled { build_loop; build_action } ->
    Build_loop.submit_rpc_request
      build_loop
      ~session_id:(Session.id session)
      ~request_id
      ~build:(build_action kind)
;;

let cancel_build_requests_for_session t session =
  match t.build with
  | Disabled -> Fiber.return ()
  | Enabled { build_loop; _ } ->
    Build_loop.cancel_rpc_requests_by_session
      build_loop
      ~session_id:(Session.Stage1.id session)
;;

let cancel_all_build_requests t =
  match t.build with
  | Disabled -> Fiber.return ()
  | Enabled { build_loop; _ } -> Build_loop.cancel_all_rpc_requests build_loop
;;

let handler (t : t Fdecl.t) action_runner_server : unit Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    t.server.clients <- Clients.add_session t.server.clients session;
    Fiber.return ()
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.server.clients <- Clients.remove_session t.server.clients session;
    cancel_build_requests_for_session t session
  in
  let rpc = Handler.create ~on_terminate ~on_init ~version:Dune_rpc.Version.latest () in
  let () =
    let module Error = Build_system_error in
    let diff ~last ~(now : Error.Set.t) =
      match last with
      | None ->
        Error.Set.current now
        |> Error.Id.Map.values
        |> List.map ~f:(fun error ->
          Diagnostics.diagnostic_event_of_error_event (Add error))
      | Some prev ->
        Error.Id.Map.merge
          (Error.Set.current prev)
          (Error.Set.current now)
          ~f:(fun _ prev now ->
            diff_map_entry
              prev
              now
              ~on_add:(fun error ->
                Diagnostics.diagnostic_event_of_error_event (Add error))
              ~on_remove:(fun error ->
                Diagnostics.diagnostic_event_of_error_event (Remove error)))
        |> Error.Id.Map.values
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.diagnostic
      (Source.Svar Build_system.errors)
      ~equal:Error.Set.equal
      ~diff
  in
  let () =
    let start_job { Running_jobs.pid; description; started_at; id } =
      let id = Running_jobs.Id.to_int id |> Job.Id.create in
      let pid = Pid.to_int pid in
      let started_at = Time.to_secs started_at in
      Job.Event.Start { Job.started_at; id; pid; description }
    in
    let stop_job id = Job.Event.Stop (Job.Id.create (Running_jobs.Id.to_int id)) in
    let stop_running_job ({ Running_jobs.id; _ } : Running_jobs.job) = stop_job id in
    let diff ~(last : Running_jobs.t option) ~(now : Running_jobs.t) =
      match last with
      | None ->
        Running_jobs.current now |> Running_jobs.Id.Map.values |> List.map ~f:start_job
      | Some last ->
        (match Running_jobs.one_event_diff ~last ~now with
         | Some event ->
           [ (match event with
              | Start job -> start_job job
              | Stop id -> stop_job id)
           ]
         | None ->
           Running_jobs.Id.Map.merge
             (Running_jobs.current last)
             (Running_jobs.current now)
             ~f:(fun _ last now ->
               diff_map_entry last now ~on_add:start_job ~on_remove:stop_running_job)
           |> Running_jobs.Id.Map.values)
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.running_jobs
      (Source.Svar Running_jobs.jobs)
      ~equal:Running_jobs.equal
      ~diff
  in
  let () =
    let diff ~last:_ ~(now : Build_system.State.t) =
      match now with
      | Initializing -> Progress.Waiting
      | Restarting_current_build -> Interrupted
      | Build_succeeded__now_waiting_for_changes -> Success
      | Build_failed__now_waiting_for_changes -> Failed
      | Building now ->
        In_progress
          { complete = now.number_of_rules_validated
          ; remaining = Build_system.Progress.number_of_rules_in_progress now
          ; failed = now.number_of_rules_failed
          }
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.progress
      (Source.Computed
         { get = (fun () -> !Build_system.state); poll_every = Time.Span.of_secs 0.2 })
      ~equal:Build_system.State.equal
      ~diff
  in
  let () =
    Handler.declare_notification rpc Procedures.Server_side.abort;
    Handler.declare_notification rpc Procedures.Server_side.log
  in
  let () = Handler.implement_request rpc Procedures.Public.ping (fun _ -> Fiber.return) in
  let implement_build_request decl ~f =
    let handler session request_id input =
      let+ outcome = submit_build_request (Fdecl.get t) session request_id (f input) in
      match outcome with
      | Success -> Dune_rpc.Build_outcome_with_diagnostics.Success
      | Failure ->
        let diagnostics =
          current_errors ()
          |> List.filter_map ~f:(fun error ->
            match Build_system_error.description error with
            | `Exn _ -> None
            | `Diagnostic compound_user_error -> Some compound_user_error)
        in
        Dune_rpc.Build_outcome_with_diagnostics.Failure diagnostics
    in
    Handler.implement_request_with_id rpc decl handler
  in
  let () =
    implement_build_request Decl.build ~f:(fun targets ->
      let targets = List.map targets ~f:Dune_rules_rpc.parse_build_arg in
      Build targets)
  in
  let () =
    implement_build_request Procedures.Public.runtest ~f:(fun paths -> Runtest paths)
  in
  let () =
    let f session request_id () =
      submit_build_request
        (Fdecl.get t)
        session
        request_id
        (Build
           [ (let alias = Dune_lang.String_with_vars.make_text Loc.none "fmt" in
              Dune_lang.Dep_conf.Alias_rec alias)
           ])
      >>| function
      (* A 'successful' formatting means there is nothing to promote. *)
      | Success -> ()
      | Failure ->
        (match
           Diff_promotion.promote_files_registered_in_last_run
             ~matching:Exact
             Dune_rpc.Files_to_promote.All
         with
         | [] -> ()
         | (_non_empty : Path.Source.t list) ->
           Code_error.raise
             "promote_files_registered_in_last_run All should always return an empty list"
             [])
    in
    Handler.implement_request_with_id rpc Procedures.Public.format f
  in
  let () =
    let f _ () =
      let t = Fdecl.get t in
      match t.server.watch_mode with
      | No -> Fiber.return `Not_in_watch_mode
      | Yes _ ->
        let+ () = Scheduler.flush_file_watcher () in
        `Ok
    in
    Handler.implement_request rpc Procedures.Public.flush_file_watcher f
  in
  let () =
    let shutdown _ () =
      let t = Fdecl.get t in
      let terminate_sessions () =
        Fiber.fork_and_join_unit
          (fun () -> cancel_all_build_requests t)
          (fun () ->
             Clients.to_list t.server.clients
             |> Fiber.parallel_iter ~f:(fun (_, (entry : Clients.entry)) ->
               Session.Stage1.close entry.session))
      in
      let shutdown () =
        let* () = stop t in
        Scheduler.shutdown `Ok;
        Fiber.return ()
      in
      Fiber.fork_and_join_unit terminate_sessions shutdown
    in
    Handler.implement_notification rpc Procedures.Public.shutdown shutdown
  in
  let () =
    let f _ () =
      let t = Fdecl.get t in
      let clients =
        Clients.to_list_map t.server.clients ~f:(fun _id (entry : Clients.entry) ->
          ( Initialize.Request.id (Session.Stage1.initialize entry.session)
          , match Session.Stage1.menu entry.session with
            | None -> Status.Menu.Uninitialized
            | Some menu -> Menu (Dune_rpc.Menu.to_list menu) ))
      in
      Fiber.return { Status.clients }
    in
    Handler.implement_request rpc Decl.status f
  in
  let () =
    let f _ () =
      current_errors () |> List.map ~f:Diagnostics.diagnostic_of_error |> Fiber.return
    in
    Handler.implement_request rpc Procedures.Public.diagnostics f
  in
  let () =
    let f _ { Dune_rpc.Promote_targets.files; matching } =
      match Diff_promotion.promote_files_registered_in_last_run ~matching files with
      | [] -> Fiber.return Dune_rpc.Build_outcome_with_diagnostics.Success
      | missing ->
        let warnings =
          List.map missing ~f:(fun fn ->
            Dune_rpc.Compound_user_error.make_with_severity
              ~main:
                (User_message.make
                   [ Pp.paragraphf
                       "Nothing to promote for %s."
                       (Path.Source.to_string_maybe_quoted fn)
                   ])
              ~related:[]
              ~severity:Dune_rpc.Diagnostic.Warning)
        in
        Fiber.return (Dune_rpc.Build_outcome_with_diagnostics.Failure warnings)
    in
    Handler.implement_request rpc Procedures.Public.promote_many f
  in
  let () =
    let f _ path =
      let files = For_handlers.source_path_of_string path in
      let _ignored : Path.Source.t list =
        Diff_promotion.promote_files_registered_in_last_run
          ~matching:Exact
          (These [ files ])
      in
      Fiber.return ()
    in
    Handler.implement_request rpc Procedures.Public.promote f
  in
  let () =
    let f _ () = Fiber.return Path.Build.(to_string root) in
    Handler.implement_request rpc Procedures.Public.build_dir f
  in
  Action_runner.implement_handler action_runner_server rpc;
  Dune_rules_rpc.register rpc;
  rpc
;;

let create ~registry ~root ~build ~where ~action_runner watch_mode =
  Global_lock.lock_exn ();
  let t = Fdecl.create Dyn.opaque in
  let action_runner, lifecycle =
    let server =
      lazy
        (let socket_file = Where.rpc_socket_file () in
         Fpath.unlink_no_err (Path.Build.to_string socket_file);
         Path.mkdir_p (Path.build (Path.Build.parent_exn socket_file));
         match Csexp_rpc.Server.create [ Where.to_socket where ] ~backlog:100 with
         | Ok s ->
           (match where with
            | `Ip _ -> Io.write_file (Path.build socket_file) (Where.to_string where)
            | `Unix _ -> ());
           at_exit (fun () -> Fpath.unlink_no_err (Path.Build.to_string socket_file));
           s
         | Error `Already_in_use ->
           User_error.raise
             [ Pp.textf
                 "Dune rpc is already running in this workspace. If this is not the \
                  case, please delete %s"
                 (Path.Build.to_string_maybe_quoted (Where.rpc_socket_file ()))
             ])
    in
    let handler = Rpc.Server.make (handler t action_runner) in
    let server = Lazy.force server in
    let lifecycle = Rpc.Server.Lifecycle.create ~handler ~root ~where ~registry ~server in
    action_runner, lifecycle
  in
  let server = { lifecycle; action_runner; watch_mode; clients = Clients.empty } in
  let res = { server; build } in
  current := Some server;
  Fdecl.set t res;
  res
;;

let run t =
  Fiber.fork_and_join_unit
    (fun () -> Rpc.Server.Lifecycle.run t.server.lifecycle)
    (fun () ->
       match t.server.action_runner with
       | None -> Fiber.return ()
       | Some runner -> Action_runner.run runner)
;;
