open Import
open Memo.O
open Dune_pkg
include Dune_pkg.Lock_dir
module Sys_vars = Pkg_sys_vars

module Load = Make_load (struct
    include Memo

    let readdir_with_kinds path =
      Readdir.read_directory_with_kinds (Path.to_string path)
      |> function
      | Error _ ->
        (* CR-someday rgrinberg: add some proper message here *)
        User_error.raise [ Pp.text "" ]
      | Ok content -> return content
    ;;

    let with_lexbuf_from_file path ~f =
      Io.Untracked.with_lexbuf_from_file path ~f |> return
    ;;
  end)

let select_lock_dir lock_dir_selection =
  let* workspace = Workspace.workspace () in
  let expander ~source pform =
    Sys_vars.expand Sys_vars.poll ~source pform
    >>| function
    | None ->
      User_error.raise
        ~loc:source.loc
        [ Pp.textf
            "Unable to compute value for variable %S"
            (Dune_sexp.Template.Pform.describe source)
        ]
    | Some variable_value -> [ Value.String variable_value ]
  in
  Workspace.Lock_dir_selection.eval lock_dir_selection ~dir:workspace.dir ~f:expander
;;

let default_dir = "dune.lock"

(* location where project lock dirs are stored *)
let path_prefix =
  (* the lock dir is always stored in the default context *)
  let ctx_name = Context_name.default |> Context_name.to_string in
  Path.Build.L.relative Private_context.t.build_dir [ ctx_name; ".lock" ]
;;

let default_path = Path.Build.relative path_prefix default_dir |> Path.build
let default_source_path = Path.Source.(relative root default_dir)

let dev_tool_to_path_segment dev_tool =
  dev_tool |> Dev_tool.package_name |> Package_name.to_string |> Path.Local.of_string
;;

(* This function returns the lock dir that is created outside the build system. *)
let dev_tool_external_lock_dir dev_tool =
  let external_root =
    Path.Build.root |> Path.build |> Path.to_absolute_filename |> Path.External.of_string
  in
  let dev_tools_path = Path.External.relative external_root ".dev-tools.locks" in
  let dev_tool_segment = dev_tool_to_path_segment dev_tool in
  Path.External.append_local dev_tools_path dev_tool_segment
;;

(* This function returns the lock dir location where the build system can create
   the lock directory. This is where lock files should be loaded from and it
   is populated either by copy rules or the solver running. *)
let dev_tool_lock_dir dev_tool =
  (* dev tools always live in default *)
  let ctx_name = Context_name.default |> Context_name.to_string in
  let dev_tool_segment = dev_tool_to_path_segment dev_tool in
  let lock_dir =
    Path.Build.L.relative Private_context.t.build_dir [ ctx_name; ".dev-tool-locks" ]
  in
  let lock_dir = Path.Build.append_local lock_dir dev_tool_segment in
  Path.build lock_dir
;;

let lock_dir_of_source p =
  let local = Path.Source.to_local p in
  Path.Build.append_local path_prefix local |> Path.build
;;

let get_source_path_for_context ctx_name =
  let* workspace = Workspace.workspace () in
  match
    List.find_map workspace.contexts ~f:(fun ctx ->
      match Context_name.equal (Workspace.Context.name ctx) ctx_name with
      | false -> None
      | true -> Some ctx)
  with
  | None | Some (Default { lock_dir = None; _ }) -> Memo.return (Some default_source_path)
  | Some (Default { lock_dir = Some lock_dir_selection; _ }) ->
    let+ source_lock_dir = select_lock_dir lock_dir_selection in
    Some source_lock_dir
  | Some (Opam _) -> Memo.return None
;;

let get_path ctx_name =
  let+ source_path = get_source_path_for_context ctx_name in
  Option.map source_path ~f:lock_dir_of_source
;;

let get_workspace_lock_dir ctx =
  let* workspace = Workspace.workspace () in
  let+ path = get_path ctx in
  let open Option.O in
  let* path = path in
  Workspace.find_lock_dir workspace path
;;

let get_with_path =
  let read_lockdir =
    Memo.exec
      (Memo.create
         ~human_readable_description:(fun p ->
           Pp.textf "read lock directory %s" (Path.to_string_maybe_quoted p)
           |> Option.some)
         "read-lock-dir"
         ~input:(module Path)
         (fun path ->
            let* () = Build_system.build_dir path in
            Load.load path))
  in
  Per_context.create_by_name ~name:"lock-dir-get" (fun ctx ->
    Memo.lazy_ ~name:"lock-dir-get" (fun () ->
      let* path =
        get_path ctx
        >>| function
        | Some p -> p
        | None ->
          Code_error.raise
            "No lock dir path for context available"
            [ "context", Context_name.to_dyn ctx ]
      in
      read_lockdir path
      >>= function
      | Error e -> Memo.return (Error e)
      | Ok lock_dir ->
        let+ workspace_lock_dir = get_workspace_lock_dir ctx in
        (match workspace_lock_dir with
         | None -> ()
         | Some workspace_lock_dir ->
           Solver_stats.Expanded_variable_bindings.validate_against_solver_env
             lock_dir.expanded_solver_variable_bindings
             (workspace_lock_dir.solver_env |> Option.value ~default:Solver_env.empty));
        Ok (path, lock_dir))
    |> Memo.Lazy.force)
  |> Staged.unstage
;;

let get ctx = get_with_path ctx >>| Result.map ~f:snd
let get_exn ctx = get ctx >>| User_error.ok_exn

let of_dev_tool dev_tool =
  let path = dev_tool |> dev_tool_external_lock_dir |> Path.external_ in
  (* Ensure the internal lock dir is built so copy rules run *)
  let* () = Build_system.build_dir (dev_tool_lock_dir dev_tool) in
  Load.load_exn path
;;

let of_dev_tool_if_lock_dir_exists dev_tool =
  let path = dev_tool |> dev_tool_external_lock_dir |> Path.external_ in
  let exists =
    (* Note we use [Fpath.exists] here rather than [Fs_memo] because a tool's
       lockdir may be generated part way through a build. *)
    Fpath.exists (Path.to_string path)
  in
  if exists
  then
    let+ t = Load.load_exn path in
    Some t
  else Memo.return None
;;

let lock_dirs_of_workspace (workspace : Workspace.t) =
  let module Set = Path.Source.Set in
  let+ lock_dirs_from_ctx =
    Memo.List.map workspace.contexts ~f:(function
      | Opam _ | Default { lock_dir = None; _ } -> Memo.return None
      | Default { lock_dir = Some selection; _ } ->
        let+ path = select_lock_dir selection in
        Some path)
    >>| List.filter_opt
  in
  match lock_dirs_from_ctx, workspace.lock_dirs with
  | [], [] -> Set.singleton default_source_path
  | lock_dirs_from_ctx, lock_dirs_from_toplevel ->
    let lock_paths_from_toplevel =
      List.map lock_dirs_from_toplevel ~f:(fun (lock_dir : Workspace.Lock_dir.t) ->
        lock_dir.path)
    in
    Set.union (Set.of_list lock_paths_from_toplevel) (Set.of_list lock_dirs_from_ctx)
;;

let lock_dir_active ctx =
  let open Memo.O in
  let* { config = { Dune_config_file.Dune_config.pkg_enabled; _ }; _ } =
    Workspace.workspace ()
  in
  match pkg_enabled with
  | Set (_, `Enabled) -> Memo.return true
  | Set (_, `Disabled) -> Memo.return false
  | Unset ->
    get_source_path_for_context ctx
    >>= (function
     | None -> Memo.return false
     | Some source -> Source_tree.find_dir source >>| Option.is_some)
;;

let source_kind (source : Dune_pkg.Source.t) =
  let loc, url = source.url in
  if OpamUrl.is_local url && url.backend = `rsync
  then (
    let path =
      match Path.of_string_allow_outside_workspace url.path with
      | External e -> Path.Outside_build_dir.External e
      | In_source_tree s -> Path.Outside_build_dir.In_source_dir s
      | In_build_dir b ->
        User_error.raise
          ~loc
          [ Pp.textf
              "package source path %s is inside the build directory"
              (Path.Build.to_string_maybe_quoted b)
          ]
    in
    Fs_memo.path_kind path
    >>| function
    | Error (ENOENT, _, _) ->
      User_error.raise
        ~loc
        [ Pp.textf
            "%s does not exist"
            (Path.Outside_build_dir.to_string_maybe_quoted path)
        ]
    | Error exn ->
      User_error.raise
        ~loc
        [ Pp.textf
            "unable to read %s"
            (Path.Outside_build_dir.to_string_maybe_quoted path)
        ; Unix_error.Detailed.pp exn
        ]
    | Ok S_REG -> `Local (`File, path)
    | Ok S_DIR -> `Local (`Directory, path)
    | Ok _kind ->
      User_error.raise
        ~loc
        [ Pp.textf
            "path %s is not a directory or a file"
            (Path.Outside_build_dir.to_string_maybe_quoted path)
        ])
  else Memo.return `Fetch
;;
