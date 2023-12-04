open Import
open Fiber.O
open Dune_rpc_server
module Global_lock = Dune_util.Global_lock

include struct
  open Dune_rpc
  module Initialize = Initialize
  module Sub = Sub
  module Progress = Progress
  module Procedures = Procedures
  module Diagnostic = Diagnostic
  module Job = Job
end

include struct
  open Dune_engine
  module Action_runner = Action_runner
  module Build_config = Build_config
  module Diff_promotion = Diff_promotion
end

include struct
  open Decl
  module Build_outcome = Build_outcome
  module Status = Status
end

module Run = struct
  module Registry = Dune_rpc_private.Registry

  module Server = Dune_rpc_server.Make (struct
      include Csexp_rpc.Session

      (* only needed for action runners. can be safely omitted elsewhere *)
      let name _ = "unnamed"
    end)

  type t =
    { handler : Dune_rpc_server.t
    ; action_runner : Action_runner.Rpc_server.t
    ; pool : Fiber.Pool.t
    ; root : string
    ; where : Dune_rpc.Where.t
    ; server : Csexp_rpc.Server.t Lazy.t
    ; stats : Dune_stats.t option
    ; server_ivar : Csexp_rpc.Server.t Fiber.Ivar.t
    ; registry : [ `Add | `Skip ]
    }

  let run t =
    let cleanup_registry = ref None in
    let with_registry f =
      match t.registry with
      | `Skip -> ()
      | `Add -> f ()
    in
    let run_cleanup_registry () =
      match !cleanup_registry with
      | None -> ()
      | Some path ->
        Fpath.unlink_no_err path;
        cleanup_registry := None
    in
    let with_print_errors f () =
      Fiber.with_error_handler f ~on_error:(fun exn ->
        Dune_console.print [ Pp.text "Uncaught RPC Error"; Exn_with_backtrace.pp exn ];
        Exn_with_backtrace.reraise exn)
    in
    let run () =
      let open Fiber.O in
      let server = Lazy.force t.server in
      let* () = Fiber.Ivar.fill t.server_ivar server in
      Fiber.fork_and_join_unit
        (fun () ->
          let* sessions = Csexp_rpc.Server.serve server in
          let () =
            with_registry
            @@ fun () ->
            let (`Caller_should_write { Registry.File.path; contents }) =
              let registry_config = Registry.Config.create (Lazy.force Dune_util.xdg) in
              let dune =
                let pid = Unix.getpid () in
                let where =
                  match t.where with
                  | `Ip (host, port) -> `Ip (host, port)
                  | `Unix a ->
                    `Unix
                      (if Filename.is_relative a
                       then Filename.concat (Sys.getcwd ()) a
                       else a)
                in
                Registry.Dune.create ~where ~root:t.root ~pid
              in
              Registry.Config.register registry_config dune
            in
            let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p (Filename.dirname path) in
            Io.String_path.write_file path contents;
            cleanup_registry := Some path;
            at_exit run_cleanup_registry
          in
          let* () = Server.serve sessions t.stats t.handler in
          Fiber.Pool.close t.pool)
        (fun () -> Fiber.Pool.run t.pool)
    in
    Fiber.finalize (with_print_errors run) ~finally:(fun () ->
      with_registry run_cleanup_registry;
      Fiber.return ())
  ;;
end

type 'a pending_build_action = Build of 'a list * Build_outcome.t Fiber.Ivar.t

module Client = Stdune.Unit

module Session_comparable = Comparable.Make (struct
    type t = Client.t Session.t

    let compare = Session.compare
    let to_dyn s = Session.to_dyn Client.to_dyn s
  end)

module Session_set = Session_comparable.Set

module Clients = struct
  type entry = { session : Client.t Session.Stage1.t }
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
end

(** Primitive unbounded FIFO channel. Reads are blocking. Writes are not
    blocking. At most one read is allowed at a time. *)
module Job_queue : sig
  type 'a t

  (** Remove the element from the internal queue without waiting for the next
      element. *)
  val pop_internal : 'a t -> 'a option

  val create : unit -> 'a t
  val read : 'a t -> 'a Fiber.t
  val write : 'a t -> 'a -> unit Fiber.t
end = struct
  (* invariant: if reader is Some then queue is empty *)
  type 'a t =
    { queue : 'a Queue.t
    ; mutable reader : 'a Fiber.Ivar.t option
    }

  let create () = { queue = Queue.create (); reader = None }
  let pop_internal t = Queue.pop t.queue

  let read t =
    Fiber.of_thunk (fun () ->
      match t.reader with
      | Some _ -> Code_error.raise "multiple concurrent reads of build job queue" []
      | None ->
        (match Queue.pop t.queue with
         | None ->
           let ivar = Fiber.Ivar.create () in
           t.reader <- Some ivar;
           Fiber.Ivar.read ivar
         | Some v -> Fiber.return v))
  ;;

  let write t elem =
    Fiber.of_thunk (fun () ->
      match t.reader with
      | Some ivar ->
        t.reader <- None;
        Fiber.Ivar.fill ivar elem
      | None ->
        Queue.push t.queue elem;
        Fiber.return ())
  ;;
end

type 'a t =
  { config : Run.t
  ; pending_build_jobs : ('a list * Build_outcome.t Fiber.Ivar.t) Job_queue.t
  ; parse_build : string -> 'a
  ; watch_mode_config : Watch_mode_config.t
  ; mutable clients : Clients.t
  }

let ready (t : _ t) =
  let* server = Fiber.Ivar.read t.config.server_ivar in
  Csexp_rpc.Server.ready server
;;

let stop (t : _ t) =
  Fiber.fork_and_join_unit
    (fun () -> Action_runner.Rpc_server.stop t.config.action_runner)
    (fun () ->
      let* server = Fiber.Ivar.peek t.config.server_ivar in
      match server with
      | None -> Fiber.return ()
      | Some server -> Csexp_rpc.Server.stop server)
;;

let handler (t : _ t Fdecl.t) action_runner_server handle : 'a Dune_rpc_server.Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    let client = () in
    t.clients <- Clients.add_session t.clients session;
    Fiber.return client
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.clients <- Clients.remove_session t.clients session;
    Fiber.return ()
  in
  let rpc =
    Handler.create ~on_terminate ~on_init ~version:Dune_rpc_private.Version.latest ()
  in
  let () =
    let module Error = Build_system_error in
    let diff ~last ~(now : Error.Set.t) =
      match last with
      | None ->
        Error.Id.Map.to_list_map (Error.Set.current now) ~f:(fun _ e ->
          Diagnostic.Event.Add (Diagnostics.diagnostic_of_error e))
      | Some (prev : Error.Set.t) ->
        (match Error.Set.one_event_diff ~prev ~next:now with
         | Some last_event -> [ Diagnostics.diagnostic_event_of_error_event last_event ]
         | _ ->
           (* the slow path where we must calculate a diff between what we have
              and the last thing we've sent to the poller *)
           Error.Id.Map.merge
             (Error.Set.current prev)
             (Error.Set.current now)
             ~f:(fun _ prev now ->
               match prev, now with
               | None, None -> assert false
               | Some prev, None ->
                 Some (Diagnostics.diagnostic_event_of_error_event (Remove prev))
               | _, Some next ->
                 Some (Diagnostics.diagnostic_event_of_error_event (Add next)))
           |> Error.Id.Map.values)
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.diagnostic
      Build_system.errors
      ~equal:Error.Set.equal
      ~diff
  in
  let () =
    let start_job { Running_jobs.pid; description; started_at; id } =
      let id = Running_jobs.Id.to_int id |> Job.Id.create in
      let pid = Pid.to_int pid in
      Job.Event.Start { Job.started_at; id; pid; description }
    in
    let stop_job id = Job.Event.Stop (Job.Id.create (Running_jobs.Id.to_int id)) in
    let diff ~(last : Running_jobs.t option) ~(now : Running_jobs.t) =
      match last with
      | None ->
        Running_jobs.current now |> Running_jobs.Id.Map.values |> List.map ~f:start_job
      | Some last ->
        (match Running_jobs.one_event_diff ~last ~now with
         | Some last_event ->
           [ (match last_event with
              | Start job -> start_job job
              | Stop id -> stop_job id)
           ]
         | None ->
           Running_jobs.Id.Map.merge
             (Running_jobs.current last)
             (Running_jobs.current now)
             ~f:(fun id last now ->
               match last, now with
               | None, None -> assert false
               | Some _, Some _ -> None
               | Some _, None -> Some (stop_job id)
               | _, Some now -> Some (start_job now))
           |> Running_jobs.Id.Map.values)
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.running_jobs
      Running_jobs.jobs
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
          { complete = now.number_of_rules_executed
          ; remaining = now.number_of_rules_discovered - now.number_of_rules_executed
          ; failed = now.number_of_rules_failed
          }
    in
    Handler.implement_long_poll
      rpc
      Procedures.Poll.progress
      Build_system.state
      ~equal:Build_system.State.equal
      ~diff
  in
  let () =
    Handler.declare_notification rpc Procedures.Server_side.abort;
    Handler.declare_notification rpc Procedures.Server_side.log
  in
  let () = Handler.implement_request rpc Procedures.Public.ping (fun _ -> Fiber.return) in
  let () =
    let build _session targets =
      let server = Fdecl.get t in
      match server.watch_mode_config with
      | No -> assert false
      | Yes Eager ->
        let error =
          Dune_rpc.Response.Error.create
            ~kind:Invalid_request
            ~message:
              "the rpc server is running with eager watch mode using --watch. to run \
               builds through an rpc client, start the server using --passive-watch-mode"
            ()
        in
        raise (Dune_rpc.Response.Error.E error)
      | Yes Passive ->
        let ivar = Fiber.Ivar.create () in
        let targets = List.map targets ~f:server.parse_build in
        let* () = Job_queue.write server.pending_build_jobs (targets, ivar) in
        Fiber.Ivar.read ivar
    in
    Handler.implement_request rpc Decl.build build
  in
  let () =
    let rec cancel_pending_jobs () =
      match Job_queue.pop_internal (Fdecl.get t).pending_build_jobs with
      | None -> Fiber.return ()
      | Some (_, job) ->
        let* () = Fiber.Ivar.fill job Build_outcome.Failure in
        cancel_pending_jobs ()
    in
    let shutdown _ () =
      let t = Fdecl.get t in
      let terminate_sessions () =
        Fiber.fork_and_join_unit cancel_pending_jobs (fun () ->
          Fiber.parallel_iter (Clients.to_list t.clients) ~f:(fun (_, entry) ->
            Session.Stage1.close entry.session))
      in
      let shutdown () =
        Fiber.fork_and_join_unit Scheduler.shutdown (fun () ->
          Csexp_rpc.Server.stop (Lazy.force t.config.server))
      in
      Fiber.fork_and_join_unit terminate_sessions shutdown
    in
    Handler.implement_notification rpc Procedures.Public.shutdown shutdown
  in
  let () =
    let f _ () =
      let t = Fdecl.get t in
      let clients =
        Clients.to_list_map t.clients ~f:(fun _id (entry : Clients.entry) ->
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
      let errors = Fiber.Svar.read Build_system.errors in
      Build_system_error.Set.current errors
      |> Build_system_error.Id.Map.values
      |> List.map ~f:Diagnostics.diagnostic_of_error
      |> Fiber.return
    in
    Handler.implement_request rpc Procedures.Public.diagnostics f
  in
  let () =
    let f _ path =
      let files = For_handlers.source_path_of_string path in
      Diff_promotion.promote_files_registered_in_last_run (These ([ files ], ignore));
      Fiber.return ()
    in
    Handler.implement_request rpc Procedures.Public.promote f
  in
  let () =
    let f _ () = Fiber.return Path.Build.(to_string root) in
    Handler.implement_request rpc Procedures.Public.build_dir f
  in
  Dune_engine.Action_runner.Rpc_server.implement_handler action_runner_server rpc;
  handle rpc;
  rpc
;;

let create
  ~lock_timeout
  ~registry
  ~root
  ~watch_mode_config
  ~handle
  stats
  action_runner
  ~parse_build
  =
  let t = Fdecl.create Dyn.opaque in
  let pending_build_jobs = Job_queue.create () in
  let handler = Dune_rpc_server.make (handler t action_runner handle) in
  let pool = Fiber.Pool.create () in
  let where = Where.default () in
  Global_lock.lock_exn ~timeout:lock_timeout;
  let server =
    lazy
      (let socket_file = Where.rpc_socket_file () in
       Path.unlink_no_err (Path.build socket_file);
       Path.mkdir_p (Path.build (Path.Build.parent_exn socket_file));
       match Csexp_rpc.Server.create [ Where.to_socket where ] ~backlog:10 with
       | Ok s ->
         (match where with
          | `Ip _ -> Io.write_file (Path.build socket_file) (Where.to_string where)
          | `Unix _ -> ());
         at_exit (fun () -> Path.Build.unlink_no_err socket_file);
         s
       | Error `Already_in_use ->
         User_error.raise
           [ Pp.textf
               "Dune rpc is already running in this workspace. If this is not the case, \
                please delete %s"
               (Path.Build.to_string_maybe_quoted (Where.rpc_socket_file ()))
           ])
  in
  let config =
    { Run.handler
    ; pool
    ; root
    ; where
    ; stats
    ; server
    ; registry
    ; action_runner
    ; server_ivar = Fiber.Ivar.create ()
    }
  in
  let res =
    { config
    ; pending_build_jobs
    ; watch_mode_config
    ; clients = Clients.empty
    ; parse_build
    }
  in
  Fdecl.set t res;
  res
;;

let run t =
  let* () = Fiber.return () in
  Fiber.fork_and_join_unit
    (fun () -> Run.run t.config)
    (fun () -> Dune_engine.Action_runner.Rpc_server.run t.config.action_runner)
;;

let action_runner t = t.config.action_runner

let pending_build_action t =
  Job_queue.read t.pending_build_jobs
  |> Fiber.map ~f:(fun (targets, ivar) -> Build (targets, ivar))
;;
