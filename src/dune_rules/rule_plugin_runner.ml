open Import
open Fiber.O
module Rpc_transport = Rpc
module Rpc = Dune_rpc.Private
module Rpc_server = Rpc_transport.Server
module Rpc_plugin = Dune_rpc_impl.Server.Rule_plugin
module Protocol = Rpc.Rule_plugin
module Name = Rule_plugin.Name
module Build = Process.Build

module Process = struct
  module Instance_id = Id.Make ()

  type t =
    { name : Name.t
    ; pid : Pid.t
    ; rpc : Rpc_plugin.t
    ; build_cancellation : Fiber.Cancel.t
    ; cancellation : Fiber.Cancel.t
    ; exited : unit Fiber.Ivar.t
    }

  let trace t event =
    Dune_trace.emit Action (fun () ->
      Dune_trace.Event.Action.Rule_plugin.event
        ~name:(Name.to_string t.name)
        ~pid:t.pid
        event)
  ;;

  let disconnected t =
    User_error.raise [ Pp.textf "Rule plugin %S disconnected." (Name.to_string t.name) ]
  ;;

  let await_initialized t =
    Rpc_plugin.initialized t.rpc
    >>= function
    | Some session -> Fiber.return session
    | None -> disconnected t
  ;;

  let stop t =
    match Fiber.Ivar.peek t.exited with
    | Some () -> Fiber.return ()
    | None ->
      if not (Fiber.Cancel.fired t.cancellation) then trace t Stop;
      let* () = Fiber.Cancel.fire t.cancellation in
      let* () = Rpc_plugin.close t.rpc in
      Fiber.Ivar.read t.exited
  ;;

  let run t =
    let connection () =
      Rpc_plugin.initialized t.rpc
      >>= function
      | None -> Fiber.return ()
      | Some session ->
        trace t Connected;
        let+ () = Rpc_server.Session.closed session in
        trace t Disconnected
    in
    let wait_for_exit () =
      (* CR-soon rgrinberg: add a timeout *)
      let* (_ : Proc.Process_info.t * Scheduler.termination_reason) =
        Scheduler.wait_for_build_process
          ~cancellation:t.cancellation
          t.pid
          ~is_process_group_leader:true
      in
      trace t Exited;
      let* () = Rpc_plugin.close t.rpc in
      Fiber.Ivar.fill t.exited ()
    in
    let run () =
      if Dune_trace.enabled Action
      then Fiber.fork_and_join_unit connection wait_for_exit
      else wait_for_exit ()
    in
    let+ (), (_ : unit Fiber.Cancel.outcome) =
      Fiber.Cancel.with_handler t.build_cancellation run ~on_cancel:(fun () -> stop t)
    in
    ()
  ;;

  let create ~build ~name ~prog ~cwd ~env =
    let id =
      let instance = Instance_id.to_int (Instance_id.gen ()) in
      Rpc.Id.make (Sexp.Atom (Printf.sprintf "dune-rules-plugin-%d" instance))
    in
    let env =
      let where =
        match Rpc_transport.Where.default () with
        | `Unix path -> `Unix (Path.reach (Path.of_string path) ~from:cwd)
        | where -> where
      in
      Rpc.Where.add_to_env where env
      |> Env.add
           ~var:(Env.Var.of_string Protocol.instance_id_env_var)
           ~value:(Csexp.to_string (Rpc.Id.to_sexp id))
    in
    let pid =
      Spawn.spawn
        ~nice:None
        ~setpgid:Spawn.Pgid.new_process_group
        ~env
        ~cwd:(Spawn.Working_dir.path (Path.to_absolute_filename cwd))
        ~prog
        ~argv0:prog
        ~args:(Array.Immutable.of_list [])
        ()
    in
    Scheduler.preserve_child_process pid;
    let exited = Fiber.Ivar.create () in
    let rpc = Rpc_plugin.register ~id ~build ~exited in
    let t =
      { name
      ; pid
      ; rpc
      ; build_cancellation = Build.cancellation build
      ; cancellation = Fiber.Cancel.create ()
      ; exited
      }
    in
    trace t Spawn;
    t
  ;;

  module Request_id = Id.Make ()

  let load_rules t arguments =
    let* result =
      Fiber.collect_errors (fun () ->
        let* session = await_initialized t in
        let id =
          Sexp.Atom (Request_id.gen () |> Request_id.to_int |> Int.to_string)
          |> Rpc.Id.make
        in
        trace t Request_start;
        Fiber.finalize_now
          ~finally:(fun () -> trace t Request_finish)
          (fun () ->
             Rpc_server.Session.request
               session
               (Rpc.Decl.Request.witness Protocol.load_rules)
               id
               arguments))
    in
    if Fiber.Cancel.fired t.cancellation || Fiber.Cancel.fired t.build_cancellation
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

  let repr = Repr.triple Path.Source.repr (Repr.abstract Context_name.to_dyn) Name.repr
  let equal = Tuple.T3.equal Path.Source.equal Context_name.equal Name.equal
  let hash = Tuple.T3.hash Path.Source.hash Context_name.hash Name.hash
  let to_dyn = Repr.to_dyn repr
end

let pool = Fiber.Pool.create ()
let processes = Table.create (module Process_key) 16
let process_mutex = Fiber.Mutex.create ()

let stop_processes () =
  let running = Table.values processes in
  Table.clear processes;
  Fiber.parallel_iter running ~f:Process.stop
;;

let get_process ~key ~name ~prog ~cwd ~env =
  Fiber.Mutex.with_lock process_mutex ~f:(fun () ->
    let build =
      match Build.get () with
      | Some build -> build
      | None ->
        Code_error.raise
          "rule plugin started outside a build"
          [ "name", Name.to_dyn name ]
    in
    let build_cancellation = Build.cancellation build in
    if Fiber.Cancel.fired build_cancellation
    then raise (Memo.Non_reproducible Scheduler.Run.Build_cancelled);
    match Table.find processes key with
    | Some ({ Process.build_cancellation = previous; _ } as process)
      when phys_equal previous build_cancellation -> Fiber.return process
    | previous ->
      let* () =
        match previous with
        | None -> Fiber.return ()
        | Some process -> Process.stop process
      in
      let process = Process.create ~build ~name ~prog ~cwd ~env in
      Table.set processes key process;
      let+ () =
        Fiber.Pool.task pool ~f:(fun () ->
          Fiber.finalize_now
            (fun () -> Process.run process)
            ~finally:(fun () ->
              match Table.find processes key with
              | Some current when phys_equal current process -> Table.remove processes key
              | None | Some _ -> ()))
      in
      process)
;;

let run () = Fiber.Pool.run pool

let stop () =
  let* () = stop_processes () in
  Fiber.Pool.close pool
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

  let find t ~project { Rule_plugin.Invocation.loc; name; arguments = _ } =
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

  let expand t ~context ~project invocation =
    let { Rule_plugin.Invocation.loc; arguments; name = _ } = invocation in
    let { dir = definition_dir
        ; definition =
            { Rule_plugin.Definition.loc = definition_loc
            ; name = definition_name
            ; executable = definition_executable
            }
        ; project = _
        }
      =
      find t ~project invocation
    in
    let* host = Context.DB.get context >>= Context.host in
    let executable =
      let source_executable =
        Path.Source.relative
          ~error_loc:definition_loc
          definition_dir
          definition_executable
      in
      Path.Build.append_source
        (Context_name.build_dir (Context.name host))
        source_executable
    in
    let* () = Build_system.build_file (Path.build executable) in
    let* process =
      let key = Dune_project.root project, context, definition_name in
      let prog = Path.to_absolute_filename (Path.build executable) in
      let cwd = Path.source definition_dir in
      let* env = Context.installed_env host in
      get_process ~key ~name:definition_name ~prog ~cwd ~env |> Memo.of_reproducible_fiber
    in
    let+ response =
      List.map arguments ~f:Dune_lang.Ast.remove_locs
      |> Process.load_rules process
      |> Memo.of_reproducible_fiber
    in
    List.map response ~f:(Dune_lang.Ast.add_loc ~loc)
  ;;
end
