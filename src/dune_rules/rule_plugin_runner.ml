open Import
open Fiber.O
module Rpc_transport = Rpc
module Rpc = Dune_rpc.Private
module Rpc_server = Rpc_transport.Server
module Csexp_rpc = Rpc_transport.Csexp_rpc
module Protocol = Rpc.Rule_plugin
module Plugin_sexp = Protocol.Sexp
module Name = Rule_plugin.Name
module Build = Process.Build

let trace_rule_plugins = Config.make_toggle ~name:"trace_rule_plugins" ~default:`Disabled

module Process = struct
  type status =
    | Starting of unit Fiber.Ivar.t
    | Initialized of unit Rpc_server.Session.t
    | Closed

  type t =
    { name : Name.t
    ; pid : Pid.t
    ; listener : Csexp_rpc.Server.t
    ; mutable status : status
    ; build_cancellation : Fiber.Cancel.t option
    ; cancellation : Fiber.Cancel.t
    ; exited : unit Fiber.Ivar.t
    ; request_mutex : Fiber.Mutex.t
    }

  let trace t event =
    match Config.get trace_rule_plugins with
    | `Disabled -> ()
    | `Enabled ->
      Dune_trace.emit Action (fun () ->
        Dune_trace.Event.Action.Rule_plugin.event
          ~name:(Name.to_string t.name)
          ~pid:t.pid
          event)
  ;;

  let disconnected t =
    User_error.raise [ Pp.textf "Rule plugin %S disconnected." (Name.to_string t.name) ]
  ;;

  let mark_closed t =
    match t.status with
    | Closed -> Fiber.return ()
    | Starting ready ->
      t.status <- Closed;
      Fiber.Ivar.fill ready ()
    | Initialized _ ->
      t.status <- Closed;
      Fiber.return ()
  ;;

  let await_initialized t =
    match t.status with
    | Closed -> disconnected t
    | Initialized session -> Fiber.return session
    | Starting ready ->
      let+ () = Fiber.Ivar.read ready in
      (match t.status with
       | Initialized session -> session
       | Closed -> disconnected t
       | Starting _ ->
         Code_error.raise
           "rule plugin initialization finished without a session"
           [ "name", Name.to_dyn t.name ])
  ;;

  let ready t session () =
    match t.status with
    | Closed -> disconnected t
    | Initialized _ ->
      User_error.raise
        [ Pp.textf "Rule plugin %S connected more than once." (Name.to_string t.name) ]
    | Starting ready ->
      t.status <- Initialized session;
      trace t Connected;
      Fiber.Ivar.fill ready ()
  ;;

  let handler t =
    let on_init _ (_ : Rpc.Initialize.Request.t) = Fiber.return () in
    let on_terminate _ =
      trace t Disconnected;
      let* () = mark_closed t in
      Csexp_rpc.Server.stop t.listener
    in
    let handler =
      Rpc_server.Handler.create ~on_init ~on_terminate ~version:Rpc.Version.latest ()
    in
    Rpc_server.Handler.declare_request handler Protocol.load_rules;
    Rpc_server.Handler.implement_request handler Protocol.ready (ready t);
    Rpc_server.make handler
  ;;

  let stop t =
    if not (Fiber.Cancel.fired t.cancellation) then trace t Stop;
    let* () = Fiber.Cancel.fire t.cancellation in
    let* () =
      match t.status with
      | Starting _ | Closed -> Csexp_rpc.Server.stop t.listener
      | Initialized session -> Rpc_server.Session.close session
    in
    Fiber.Ivar.read t.exited
  ;;

  let run t =
    let serve () =
      if Fiber.Cancel.fired t.cancellation
      then Fiber.return ()
      else
        let* sessions = Csexp_rpc.Server.serve t.listener in
        Rpc_server.serve sessions (handler t)
    in
    let run () =
      Fiber.fork_and_join_unit serve (fun () ->
        let* (_ : Proc.Process_info.t * Scheduler.termination_reason) =
          Scheduler.wait_for_build_process
            ~cancellation:t.cancellation
            t.pid
            ~is_process_group_leader:true
        in
        trace t Exited;
        let* () = mark_closed t in
        let* () = Csexp_rpc.Server.stop t.listener in
        Fiber.Ivar.fill t.exited ())
    in
    match t.build_cancellation with
    | None -> run ()
    | Some cancellation ->
      let+ (), _ =
        Fiber.Cancel.with_handler cancellation run ~on_cancel:(fun () -> stop t)
      in
      ()
  ;;

  let port listener =
    match Csexp_rpc.Server.listening_address listener with
    | [ Unix.ADDR_INET (_, port) ] -> port
    | addresses ->
      Code_error.raise
        "unexpected rule plugin listener addresses"
        [ "count", Dyn.int (List.length addresses) ]
  ;;

  let create ~build_cancellation ~name ~prog ~cwd ~env =
    let listener =
      match
        Csexp_rpc.Server.create [ Unix.ADDR_INET (Unix.inet_addr_loopback, 0) ] ~backlog:1
      with
      | Ok listener -> listener
      | Error `Already_in_use ->
        Code_error.raise "failed to allocate a rule plugin listener" []
    in
    let env =
      Env.add
        env
        ~var:(Env.Var.of_string "DUNE_RULES_PLUGIN_HOST")
        ~value:(Unix.string_of_inet_addr Unix.inet_addr_loopback)
      |> Env.add
           ~var:(Env.Var.of_string "DUNE_RULES_PLUGIN_PORT")
           ~value:(Int.to_string (port listener))
    in
    let pid =
      Spawn.spawn
        ~setpgid:Spawn.Pgid.new_process_group
        ~env
        ~cwd:(Spawn.Working_dir.path cwd)
        ~prog
        ~argv0:prog
        ~args:(Array.Immutable.of_list [])
        ()
    in
    Scheduler.preserve_child_process pid;
    let t =
      { name
      ; pid
      ; listener
      ; status = Starting (Fiber.Ivar.create ())
      ; build_cancellation
      ; cancellation = Fiber.Cancel.create ()
      ; exited = Fiber.Ivar.create ()
      ; request_mutex = Fiber.Mutex.create ()
      }
    in
    trace t Spawn;
    t
  ;;

  module Request_id = Id.Make ()

  let load_rules t arguments =
    let* result =
      Fiber.collect_errors (fun () ->
        Fiber.Mutex.with_lock t.request_mutex ~f:(fun () ->
          let* session = await_initialized t in
          let id =
            Request_id.gen ()
            |> Request_id.to_int
            |> Int.to_string
            |> fun id -> Rpc.Id.make (Sexp.Atom id)
          in
          trace t Request_start;
          Fiber.finalize_now
            (fun () ->
               Rpc_server.Session.request
                 session
                 (Rpc.Decl.Request.witness Protocol.load_rules)
                 id
                 arguments)
            ~finally:(fun () -> trace t Request_finish)))
    in
    let build_cancelled =
      match t.build_cancellation with
      | None -> false
      | Some cancellation -> Fiber.Cancel.fired cancellation
    in
    if Fiber.Cancel.fired t.cancellation || build_cancelled
    then
      let* () = stop t in
      raise (Memo.Non_reproducible Scheduler.Run.Build_cancelled)
    else (
      match result with
      | Ok response -> Fiber.return response
      | Error exns -> Fiber.reraise_all exns)
  ;;
end

module Process_key = struct
  type t = Path.Source.t * Context_name.t * Name.t

  let equal = Tuple.T3.equal Path.Source.equal Context_name.equal Name.equal
  let hash = Tuple.T3.hash Path.Source.hash Context_name.hash Name.hash

  let to_dyn =
    Repr.to_dyn
      (Repr.triple Path.Source.repr (Repr.abstract Context_name.to_dyn) Name.repr)
  ;;
end

let pool = Fiber.Pool.create ()
let processes = Table.create (module Process_key) 16
let process_mutex = Fiber.Mutex.create ()
let current_run = ref None

let stop_processes () =
  let running = Table.values processes in
  Table.clear processes;
  Fiber.parallel_iter running ~f:Process.stop
;;

let switch_run run =
  match !current_run with
  | Some previous when phys_equal previous run -> Fiber.return ()
  | None ->
    current_run := Some run;
    Fiber.return ()
  | Some _ ->
    let* () = stop_processes () in
    current_run := Some run;
    Fiber.return ()
;;

let get_process ~run ~key ~name ~prog ~cwd ~env =
  Fiber.Mutex.with_lock process_mutex ~f:(fun () ->
    let* () = switch_run run in
    let build_cancellation =
      match Build.get () with
      | None -> None
      | Some build ->
        let cancellation = Build.cancellation build in
        if Fiber.Cancel.fired cancellation
        then raise (Memo.Non_reproducible Scheduler.Run.Build_cancelled);
        Some cancellation
    in
    match Table.find processes key with
    | Some ({ Process.build_cancellation = previous; _ } as process)
      when Option.equal phys_equal previous build_cancellation -> Fiber.return process
    | previous ->
      let* () =
        match previous with
        | None -> Fiber.return ()
        | Some process -> Process.stop process
      in
      let process = Process.create ~build_cancellation ~name ~prog ~cwd ~env in
      Table.set processes key process;
      let+ () = Fiber.Pool.task pool ~f:(fun () -> Process.run process) in
      process)
;;

let run () = Fiber.Pool.run pool

let stop () =
  let* () = stop_processes () in
  Fiber.Pool.close pool
;;

let rec sexp_of_ast = function
  | Dune_lang.Ast.Atom (_, A atom) -> Plugin_sexp.Atom atom
  | Quoted_string (_, string) -> Quoted_string string
  | Template { quoted; parts; _ } ->
    let parts =
      List.map parts ~f:(function
        | Dune_lang.Template.Part.Text text -> Plugin_sexp.Text text
        | Pform { name; payload; _ } ->
          let payload =
            Option.map payload ~f:Dune_lang.Template.Pform.Payload.to_string
          in
          Pform { Plugin_sexp.name; payload })
    in
    Template { Plugin_sexp.quoted; parts }
  | List (_, sexps) -> List (List.map sexps ~f:sexp_of_ast)
;;

let rec ast_of_sexp ~loc ~plugin (sexp : Plugin_sexp.t) =
  match sexp with
  | Atom atom ->
    (match Dune_lang.Atom.parse atom with
     | Some atom -> Dune_lang.Ast.Atom (loc, atom)
     | None ->
       User_error.raise
         ~loc
         [ Pp.textf
             "Rule plugin %S returned an invalid s-expression:"
             (Name.to_string plugin)
         ; Pp.verbatim (Conv.to_sexp Protocol.sexp sexp |> Csexp.to_string)
         ])
  | Quoted_string string -> Dune_lang.Ast.Quoted_string (loc, string)
  | Template { quoted; parts } ->
    let parts =
      List.map parts ~f:(function
        | Plugin_sexp.Text text -> Dune_lang.Template.Part.Text text
        | Pform { name; payload } ->
          let payload =
            Option.map payload ~f:Dune_lang.Template.Pform.Payload.of_string
          in
          Pform { Dune_lang.Template.Pform.loc; name; payload })
    in
    Template { Dune_lang.Template.quoted; parts; loc }
  | List sexps -> Dune_lang.Ast.List (loc, List.map sexps ~f:(ast_of_sexp ~loc ~plugin))
;;

module Registry = struct
  open Memo.O

  type entry =
    { dir : Path.Source.t
    ; project : Dune_project.t
    ; definition : Rule_plugin.Definition.t
    }

  type t = entry Name.Map.t Path.Source.Map.t

  let create entries =
    List.fold_left
      entries
      ~init:Path.Source.Map.empty
      ~f:(fun projects ({ project; definition; _ } as entry) ->
        let root = Dune_project.root project in
        let plugins =
          Option.value (Path.Source.Map.find projects root) ~default:Name.Map.empty
        in
        let plugins =
          match Name.Map.add plugins definition.name entry with
          | Ok plugins -> plugins
          | Error previous ->
            User_error.raise
              ~loc:definition.loc
              [ Pp.textf
                  "Rule plugin %S is defined more than once."
                  (Name.to_string definition.name)
              ; Pp.textf
                  "The previous definition is at %s."
                  (Loc.to_file_colon_line previous.definition.loc)
              ]
        in
        Path.Source.Map.set projects root plugins)
  ;;

  let find t ~project { Rule_plugin.Invocation.loc; name; _ } =
    let found =
      let root = Dune_project.root project in
      match Path.Source.Map.find t root with
      | None -> None
      | Some plugins -> Name.Map.find plugins name
    in
    match found with
    | Some definition -> definition
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Rule plugin %S is not defined in this project." (Name.to_string name)
        ]
  ;;

  let expand t ~context ~project ~dir:invocation_dir invocation =
    let { Rule_plugin.Invocation.loc; name; arguments } = invocation in
    let { dir = definition_dir; definition; _ } = find t ~project invocation in
    let { Rule_plugin.Definition.loc = definition_loc
        ; name = definition_name
        ; executable = definition_executable
        }
      =
      definition
    in
    if Path.Source.equal definition_dir invocation_dir
    then
      User_error.raise
        ~loc
        [ Pp.textf
            "Rule plugin %S cannot be used in the directory where it is defined."
            (Name.to_string name)
        ];
    let source_executable =
      Path.Source.relative ~error_loc:definition_loc definition_dir definition_executable
    in
    let* context_value = Context.DB.get context in
    let* host = Context.host context_value in
    let executable =
      Path.Build.append_source
        (Context_name.build_dir (Context.name host))
        source_executable
    in
    let* () = Build_system.build_file (Path.build executable) in
    let* env = Context.installed_env host in
    let* run = Memo.current_run () in
    let key = Dune_project.root project, context, definition_name in
    let prog = Path.to_absolute_filename (Path.build executable) in
    let cwd = Path.to_absolute_filename (Path.source definition_dir) in
    let* process =
      get_process ~run ~key ~name:definition_name ~prog ~cwd ~env
      |> Memo.of_reproducible_fiber
    in
    let arguments = List.map arguments ~f:sexp_of_ast in
    let+ response = Process.load_rules process arguments |> Memo.of_reproducible_fiber in
    List.map response ~f:(ast_of_sexp ~loc ~plugin:name)
  ;;
end
