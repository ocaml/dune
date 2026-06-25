open Import
open Action.Ext.Exec

include struct
  open Dune_rpc
  module Action_id = Action_id
  module Action_plugin = Action_plugin
end

let to_dune_dep_set =
  let of_action_plugin_dep ~loc ~working_dir : Dune_rpc.Dep.t -> Dep.t =
    let to_dune_path = Path.relative working_dir in
    function
    | File fn -> Dep.file (to_dune_path fn)
    | Directory dir ->
      let selector =
        let dir = to_dune_path dir in
        File_selector.of_glob ~dir Glob.universal
      in
      Dep.file_selector selector
    | Glob { path; glob } ->
      let selector =
        let dir = to_dune_path path in
        let glob = Glob.of_string_exn loc glob in
        File_selector.of_glob ~dir glob
      in
      Dep.file_selector selector
  in
  fun set ~loc ~working_dir ->
    Dune_rpc.Dep.Set.to_list_map set ~f:(of_action_plugin_dep ~loc ~working_dir)
    |> Dep.Set.of_list
;;

module Server = struct
  module Rpc = Action_plugin.Rpc
  module Build_deps = Dune_rpc.Procedures.Public.Action_plugin.Build_deps
  module Handler = Root.Rpc.Server.Handler

  type active =
    { build_deps : Dep.Set.t -> unit Fiber.t
    ; rule_loc : Loc.t
    ; working_dir : Path.t
    ; mutable initialized : bool
    }

  let active = Action_id.Table.create 16

  let find_active action_id =
    match Action_id.Table.find active action_id with
    | Some active -> active
    | None ->
      let message =
        Printf.sprintf "unknown dynamic action %S" (Action_id.to_string action_id)
      in
      raise
        (Dune_rpc.Response.Error.E
           (Dune_rpc.Response.Error.create
              ~kind:Dune_rpc.Response.Error.Invalid_request
              ~message
              ()))
  ;;

  let with_active ~(ectx : context) ~(eenv : env) f =
    let action_id = Action_id.gen () in
    let active_action =
      { build_deps = ectx.build_deps
      ; rule_loc = ectx.rule_loc
      ; working_dir = eenv.working_dir
      ; initialized = false
      }
    in
    Action_id.Table.add_exn active action_id active_action;
    Fiber.finalize
      (fun () -> f action_id active_action)
      ~finally:(fun () ->
        Action_id.Table.remove active action_id;
        Fiber.return ())
  ;;

  let build_deps =
    let build_error_message =
      let rec exception_message = function
        | User_error.E message -> User_message.to_string message
        | Memo.Error.E error -> exception_message (Memo.Error.get error)
        | exn -> Printexc.to_string exn
      in
      function
      | [] -> "dependency build failed"
      | { Exn_with_backtrace.exn; _ } :: _ -> exception_message exn
    in
    fun _session { Build_deps.action_id; deps } ->
      let active = find_active action_id in
      let deps_to_build =
        to_dune_dep_set deps ~loc:active.rule_loc ~working_dir:active.working_dir
      in
      let open Fiber.O in
      Fiber.collect_errors (fun () -> active.build_deps deps_to_build)
      >>| function
      | Error errors -> Some (build_error_message errors)
      | Ok () -> None
  ;;

  let initialize _session action_id =
    let active = find_active action_id in
    active.initialized <- true;
    Fiber.return ()
  ;;

  let implement_handler handler =
    Handler.implement_request handler Rpc.initialize initialize;
    Handler.implement_request handler Rpc.build_deps build_deps
  ;;
end

let exec ~(ectx : context) ~(eenv : env) prog args =
  let open Fiber.O in
  let prog_name = Path.reach ~from:eenv.working_dir prog in
  Server.with_active ~ectx ~eenv (fun action_id active_action ->
    let env =
      let where =
        match Root.Rpc.Where.default () with
        | `Unix _ ->
          `Unix
            (Path.reach
               (Path.build (Root.Rpc.Where.rpc_socket_file ()))
               ~from:eenv.working_dir)
        | where -> where
      in
      Dune_rpc.Where.add_to_env where eenv.env
      |> Env.add
           ~var:Action_plugin.Rpc.action_id_env_variable
           ~value:(Action_id.to_string action_id)
    in
    let+ () =
      Process.run
        ~display:!Clflags.display
        Strict
        ~dir:eenv.working_dir
        ~env
        ~stderr_to:eenv.stderr_to
        ~stdin_from:eenv.stdin_from
        ~metadata:ectx.metadata
        prog
        args
    in
    if not active_action.initialized
    then
      User_error.raise
        ~loc:ectx.rule_loc
        [ Pp.textf
            "Executable '%s' declared as using dune-action-plugin (declared with \
             'dynamic-run' tag) failed to respond to dune."
            prog_name
        ; Pp.nop
        ; Pp.text
            "If you don't use dynamic dependency discovery in your executable you may \
             consider changing 'dynamic-run' to 'run' in your rule definition."
        ];
    ())
;;

module Spec = struct
  type ('path, 'target) t = ('path, Action.Prog.Not_found.t) result * string list

  let name = "dynamic-run"
  let version = 1
  let is_useful_to ~memoize = memoize
  let is_dynamic = true
  let runs_process = true
  let can_run_in_action_runner = true

  let encode (prog, args) f _ : Sexp.t =
    let open Sexp in
    List
      [ Atom name
      ; Atom (Int.to_string version)
      ; (match prog with
         | Ok path -> f path
         | Error error -> Atom (Filename.to_string (Action.Prog.Not_found.program error)))
      ; List (List.map args ~f:(fun s -> Atom s))
      ]
  ;;

  let bimap (prog, args) f _ = Result.map ~f prog, args

  let action (prog, args) ~ectx ~eenv =
    match prog with
    | Error e -> Action.Prog.Not_found.raise e
    | Ok prog -> exec ~ectx ~eenv prog args
  ;;
end

let action ~prog ~args =
  let module M = struct
    type path = Path.t
    type target = Path.Build.t

    module Spec = Spec

    let v = prog, args
  end
  in
  Action.Extension (module M)
;;
