open Import
open Fiber.O

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
  module Build_config = Build_config
  module Diff_promotion = Diff_promotion
  module Build_outcome = Scheduler.Run.Build_outcome
end

include struct
  open Decl
  module Status = Status
end

module Session = Rpc.Server.Session
module Handler = Rpc.Server.Handler
module Csexp_rpc = Rpc.Csexp_rpc

module Run = struct
  type t =
    { handler : Rpc.Server.t
    ; pool : Fiber.Pool.t
    ; root : string
    ; where : Dune_rpc.Where.t
    ; server : Csexp_rpc.Server.t Lazy.t
    ; startup_ivar : (Csexp_rpc.Server.t, Exn_with_backtrace.t) result Fiber.Ivar.t
    ; registry : [ `Add | `Skip ]
    }

  let run t =
    let registry = Rpc.Registry.create ~root:t.root ~where:t.where t.registry in
    let with_print_errors f () =
      Fiber.with_error_handler f ~on_error:(fun exn ->
        Console.print [ Pp.text "Uncaught RPC Error"; Exn_with_backtrace.pp exn ];
        Exn_with_backtrace.reraise exn)
    in
    let run () =
      let open Fiber.O in
      let* server =
        match Exn_with_backtrace.try_with (fun () -> Lazy.force t.server) with
        | Ok server ->
          let+ () = Fiber.Ivar.fill t.startup_ivar (Ok server) in
          server
        | Error exn ->
          let () =
            Dune_trace.emit Rpc (fun () -> Dune_trace.Event.Rpc.startup_failure exn)
          in
          let* () = Fiber.Ivar.fill t.startup_ivar (Error exn) in
          Exn_with_backtrace.reraise exn
      in
      Fiber.fork_and_join_unit
        (fun () ->
           let* sessions = Csexp_rpc.Server.serve server in
           let () = Rpc.Registry.register registry in
           let* () = Rpc.Server.serve sessions t.handler in
           Fiber.Pool.close t.pool)
        (fun () -> Fiber.Pool.run t.pool)
    in
    Fiber.finalize (with_print_errors run) ~finally:(fun () ->
      Rpc.Registry.cleanup registry;
      Fiber.return ())
  ;;
end

type 'build_arg pending_action_kind =
  | Build of 'build_arg list
  | Runtest of string list

type 'build_arg pending_action =
  { kind : 'build_arg pending_action_kind
  ; outcome : Scheduler.Run.Build_outcome.t Fiber.Ivar.t
  }

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

type 'build_arg t =
  { config : Run.t
  ; pending_jobs : 'build_arg pending_action Job_queue.t
  ; mutable clients : Clients.t
  }

let ready (t : _ t) =
  Fiber.Ivar.read t.config.startup_ivar
  >>= function
  | Ok server -> Csexp_rpc.Server.ready server
  | Error _exn -> raise Dune_util.Report_error.Already_reported
;;

let stop (t : _ t) =
  Fiber.Ivar.peek t.config.startup_ivar
  >>= function
  | None -> Fiber.return ()
  | Some (Error _) -> Fiber.return ()
  | Some (Ok server) -> Csexp_rpc.Server.stop server
;;

let get_current_diagnostic_errors () =
  Fiber.Svar.read Build_system.errors
  |> Build_system_error.Set.current
  |> Build_system_error.Id.Map.values
  |> List.filter_map ~f:(fun error ->
    match Build_system_error.description error with
    | `Exn _ -> None
    | `Diagnostic compound_user_error -> Some compound_user_error)
;;

let handler (t : _ t Fdecl.t) : 'build_arg Handler.t =
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
  let rpc = Handler.create ~on_terminate ~on_init ~version:Dune_rpc.Version.latest () in
  let () =
    let module Error = Build_system_error in
    let diff ~last ~(now : Error.Set.t) =
      match last with
      | None ->
        Error.Id.Map.to_list_map (Error.Set.current now) ~f:(fun _ e ->
          Diagnostic.Event.Add (Diagnostics.diagnostic_of_error e))
      | Some prev ->
        Error.Id.Map.merge
          (Error.Set.current prev)
          (Error.Set.current now)
          ~f:(fun _ prev now ->
            match prev, now with
            | None, None -> assert false
            | Some prev, None ->
              Some (Diagnostics.diagnostic_event_of_error_event (Remove prev))
            | None, Some next ->
              Some (Diagnostics.diagnostic_event_of_error_event (Add next))
            | Some _, Some _ -> None)
        |> Error.Id.Map.values
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
      let started_at = Time.to_secs started_at in
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
  let implement_request_pending_action decl ~f =
    let handler _session input =
      let outcome = Fiber.Ivar.create () in
      let* () =
        let server = Fdecl.get t in
        Job_queue.write server.pending_jobs { kind = f input; outcome }
      in
      Fiber.Ivar.read outcome
      >>| function
      | Success -> Dune_rpc.Build_outcome_with_diagnostics.Success
      | Failure -> Failure (get_current_diagnostic_errors ())
    in
    Handler.implement_request rpc decl handler
  in
  let () =
    implement_request_pending_action Decl.build ~f:(fun targets ->
      let targets = List.map targets ~f:Dune_rules_rpc.parse_build_arg in
      Build targets)
  in
  let () =
    implement_request_pending_action Procedures.Public.runtest ~f:(fun paths ->
      Runtest paths)
  in
  let () =
    let f _ () =
      let outcome = Fiber.Ivar.create () in
      let* () =
        let server = Fdecl.get t in
        let target =
          Dune_lang.Dep_conf.Alias_rec
            (Dune_lang.String_with_vars.make_text Loc.none "fmt")
        in
        Job_queue.write server.pending_jobs { kind = Build [ target ]; outcome }
      in
      Fiber.Ivar.read outcome
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
    Handler.implement_request rpc Procedures.Public.format f
  in
  let () =
    let rec cancel_pending_jobs () =
      match Job_queue.pop_internal (Fdecl.get t).pending_jobs with
      | None -> Fiber.return ()
      | Some { kind = _; outcome } ->
        let* () = Fiber.Ivar.fill outcome Build_outcome.Failure in
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
        let* () = Csexp_rpc.Server.stop (Lazy.force t.config.server) in
        Scheduler.shutdown ();
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
  Dune_rules_rpc.register rpc;
  rpc
;;

let create ~lock_timeout ~registry ~root =
  let where = Where.default () in
  Global_lock.lock_exn ~timeout:lock_timeout;
  let t = Fdecl.create Dyn.opaque in
  let config =
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
    let handler = Rpc.Server.make (handler t) in
    { Run.handler
    ; pool = Fiber.Pool.create ()
    ; root
    ; where
    ; server
    ; registry
    ; startup_ivar = Fiber.Ivar.create ()
    }
  in
  let res =
    let pending_jobs = Job_queue.create () in
    { config; pending_jobs; clients = Clients.empty }
  in
  Fdecl.set t res;
  res
;;

let run t =
  let* () = Fiber.return () in
  Run.run t.config
;;

let pending_action t = Job_queue.read t.pending_jobs
