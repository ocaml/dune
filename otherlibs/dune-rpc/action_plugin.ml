open Import
module Glob = Glob
module Build_deps = Procedures.Public.Action_plugin.Build_deps
module Initialize_response = Procedures.Public.Action_plugin.Initialize_response
module Path = Stdune.Path

type action_id = Action_id.t

module Error = struct
  exception E of string

  let raise string = raise (E string)
end

let validate_path path =
  if not (Filename.is_relative path)
  then
    invalid_arg
      (Printf.sprintf
         "Path %S is absolute. All paths used with Dune_rpc.V1.Action_plugin must be \
          relative."
         path)
;;

(* Unlike Path.External.reach, the wire namespace requires relative paths on
   Windows too. Split off the volume before using the local-path operation. *)
let relative_dir ~root dir =
  let split dir =
    let rec loop dir components =
      let parent = Filename.dirname dir in
      if String.equal parent dir
      then (
        let volume =
          if Sys.win32
          then
            String.map dir ~f:(function
              | '\\' -> '/'
              | c -> c)
            |> String.lowercase_ascii
          else dir
        in
        volume, Path.Local.of_comps components)
      else loop parent (Filename.of_string_exn (Filename.basename dir) :: components)
    in
    loop dir []
  in
  let root_volume, root = split root in
  let volume, dir = split dir in
  if not (String.equal root_volume volume)
  then Error.raise "The current directory and action root are on different volumes.";
  Path.Local.reach dir ~from:root
;;

module type Rpc_client = Client.Public

module Rpc = struct
  let action_id_env_variable = Env.Var.of_string "DUNE_DYNAMIC_RUN_ACTION_ID"
  let initialize = Procedures.Public.Action_plugin.initialize
  let build_deps = Procedures.Public.Action_plugin.build_deps
  let initialize_request = initialize.decl
  let build_deps_request = build_deps.decl
end

module Make
    (Fiber : Fiber_intf.S)
    (Chan : sig
       type t
     end)
    (Client : Rpc_client with type 'a fiber := 'a Fiber.t and type chan := Chan.t) =
struct
  open Fiber.O

  type t =
    | Outside_of_dune
    | Under_dune of
        { client : Client.t
        ; action_id : action_id
        ; root : string
        ; build_deps_request : (Build_deps.t, string option) Client.Versioned.request
        }

  let prepare_request client request =
    let* result = Client.Versioned.prepare_request client request in
    match result with
    | Ok request -> Fiber.return request
    | Error error -> Error.raise (Version_error.message error)
  ;;

  let request client request payload =
    let* result = Client.request client request payload in
    match result with
    | Ok response -> Fiber.return response
    | Error error -> Error.raise ("dune rpc error: " ^ Types.Response.Error.message error)
  ;;

  let create client ~action_id =
    let* initialize_request = prepare_request client Rpc.initialize_request in
    let* build_deps_request = prepare_request client Rpc.build_deps_request in
    let* { Initialize_response.root } = request client initialize_request action_id in
    Fiber.return (Under_dune { client; action_id; root; build_deps_request })
  ;;

  let run chan ~action_id ~f =
    let id = Types.Id.make (List [ Atom "dap"; Atom (Action_id.to_string action_id) ]) in
    let initialize = Types.Initialize.Request.create ~id in
    Client.connect chan initialize ~f:(fun client ->
      let* t = create client ~action_id in
      f t)
  ;;

  let outside_of_dune = Outside_of_dune

  let build_deps t deps =
    match t with
    | Outside_of_dune -> Fiber.return ()
    | Under_dune { client; action_id; build_deps_request; root = _ } ->
      let* response = request client build_deps_request { Build_deps.action_id; deps } in
      (match response with
       | None -> Fiber.return ()
       | Some message -> Error.raise message)
  ;;

  let resolve_path t path =
    validate_path path;
    let cwd = Sys.getcwd () in
    let dependency =
      match t with
      | Outside_of_dune -> path
      | Under_dune { root; _ } -> Filename.concat (relative_dir ~root cwd) path
    in
    Filename.concat cwd path, dependency
  ;;

  let read_file t ~path =
    let absolute_path, dependency = resolve_path t path in
    let* () = build_deps t (Dep.Set.singleton (Dep.File dependency)) in
    match Stdune.Io.String_path.read_file absolute_path with
    | contents -> Fiber.return contents
    | exception Unix.Unix_error (error, syscall, _) ->
      let error = Stdune.Unix_error.Detailed.create error ~syscall ~arg:path in
      Error.raise ("read_file: " ^ Stdune.Unix_error.Detailed.to_string_hum error)
    | exception Sys_error error -> Error.raise ("read_file: " ^ error)
  ;;

  let read_directory_with_glob t ~path ~glob =
    let absolute_path, dependency = resolve_path t path in
    let dep = Dep.Glob { path = dependency; glob = Glob.to_string glob } in
    let* () = build_deps t (Dep.Set.singleton dep) in
    let entries =
      match Stdune.Readdir.read_directory absolute_path with
      | Ok entries -> Stdune.Filename.L.to_string entries
      | Error ((Unix.ENOENT | ENOTDIR), _, _) -> []
      | Error (error, syscall, _) ->
        let error = Stdune.Unix_error.Detailed.create error ~syscall ~arg:path in
        Error.raise ("read_directory: " ^ Stdune.Unix_error.Detailed.to_string_hum error)
    in
    List.filter entries ~f:(Glob.test glob)
    |> List.sort ~compare:String.compare
    |> Fiber.return
  ;;
end

type run_context =
  | Outside_of_dune
  | Under_dune of
      { action_id : action_id
      ; where : Where.t
      }

let old_run_by_dune_env_variable = Env.Var.of_string "DUNE_DYNAMIC_RUN_CLIENT"

let run_context () =
  match
    ( Env.get Env.initial Rpc.action_id_env_variable
    , Env.get Env.initial old_run_by_dune_env_variable )
  with
  | None, None -> Outside_of_dune
  | None, Some _ ->
    Error.raise
      "this dune-action-plugin executable requires Dune's RPC dynamic-run protocol"
  | Some action_id, _ ->
    let where =
      match Where.of_env Env.initial with
      | Ok (`Unix socket) when not (Filename.is_relative socket) ->
        let dir = relative_dir ~root:(Sys.getcwd ()) (Filename.dirname socket) in
        `Unix (Filename.concat dir (Filename.basename socket))
      | Ok where -> where
      | Error `Missing -> Error.raise "unable to find a dune rpc server"
      | Error (`Exn exn) ->
        Error.raise ("invalid dune rpc server address: " ^ Printexc.to_string exn)
    in
    Under_dune { action_id = Action_id.of_string action_id; where }
;;
