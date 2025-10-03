open Import
open Memo.O
open Dune_pkg
include Dune_pkg.Lock_dir

module Sys_vars = struct
  type t =
    { os : string option Memo.Lazy.t
    ; os_version : string option Memo.Lazy.t
    ; os_distribution : string option Memo.Lazy.t
    ; os_family : string option Memo.Lazy.t
    ; arch : string option Memo.Lazy.t
    ; sys_ocaml_version : string option Memo.Lazy.t
    }

  let os t (v : Dune_lang.Pform.Var.Os.t) =
    Memo.Lazy.force
      (match v with
       | Os -> t.os
       | Os_version -> t.os_version
       | Os_distribution -> t.os_distribution
       | Os_family -> t.os_family)
  ;;

  let poll =
    let vars =
      lazy
        (let path = Env_path.path (Global.env ()) in
         Sys_poll.make ~path)
    in
    let sys_poll_memo key =
      Memo.lazy_ ~name:"sys-poll-vars" ~cutoff:(Option.equal String.equal) (fun () ->
        let vars = Lazy.force vars in
        Memo.of_reproducible_fiber @@ key vars)
    in
    { os = sys_poll_memo Sys_poll.os
    ; os_version = sys_poll_memo Sys_poll.os_version
    ; os_distribution = sys_poll_memo Sys_poll.os_distribution
    ; os_family = sys_poll_memo Sys_poll.os_family
    ; arch = sys_poll_memo Sys_poll.arch
    ; sys_ocaml_version = sys_poll_memo Sys_poll.sys_ocaml_version
    }
  ;;

  (* A pform expander for expanding a subset of the variables in "lang dune" (ie. the
     same variables available in dune files) based on the OPAM variables polled
     by this module. OPAM variables are converted to their equivalent dune
     values, for example "os = macos" will be converted to "system = macosx". *)
  let expand t ~(source : Dune_sexp.Template.Pform.t) (pform : Pform.t) =
    match pform with
    | Macro _ ->
      User_error.raise
        ~loc:source.loc
        [ Pp.text "Macros are not allowed in this position." ]
    | Var var ->
      (* Convert values into the corresponding values that would be returned by
         "ocamlc -config". We can't actually run "ocamlc -config" here because
         ocamlc might not be installed yet. *)
      (match var with
       | Architecture ->
         let+ arch = Memo.Lazy.force t.arch in
         (match arch with
          | Some "x86_64" -> Some "amd64"
          | other -> other)
       | System ->
         let+ os = Memo.Lazy.force t.os in
         (match os with
          | Some "macos" -> Some "macosx"
          | other -> other)
       | _ ->
         User_error.raise
           ~loc:source.loc
           [ Pp.textf
               "%s isn't allowed in this position."
               (Dune_sexp.Template.Pform.describe source)
           ])
  ;;

  let solver_env () =
    let open Memo.O in
    let module V = Package_variable_name in
    let { os; os_version; os_distribution; os_family; arch; sys_ocaml_version } = poll in
    let+ var_value_pairs =
      [ V.os, os
      ; V.os_version, os_version
      ; V.os_distribution, os_distribution
      ; V.os_family, os_family
      ; V.arch, arch
      ; V.sys_ocaml_version, sys_ocaml_version
      ]
      |> Memo.List.filter_map ~f:(fun (var, value) ->
        let+ value = Memo.Lazy.force value in
        Option.map value ~f:(fun value -> var, Variable_value.string value))
    in
    List.fold_left var_value_pairs ~init:Solver_env.empty ~f:(fun acc (var, value) ->
      Solver_env.set acc var value)
  ;;
end

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

let dev_tool_source_lock_dir dev_tool =
  let dev_tools_path = Path.Source.(relative root "dev-tools.locks") in
  let dev_tool_segment = dev_tool_to_path_segment dev_tool in
  Path.Source.append_local dev_tools_path dev_tool_segment
;;

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

let get_paths ctx_name =
  let* workspace = Workspace.workspace () in
  let ctx =
    List.find_map workspace.contexts ~f:(fun ctx ->
      match Context_name.equal (Workspace.Context.name ctx) ctx_name with
      | false -> None
      | true -> Some ctx)
  in
  let* lock_dir_paths =
    match ctx with
    | None | Some (Default { lock_dir = None; _ }) ->
      Memo.return (Some (default_source_path, default_path))
    | Some (Default { lock_dir = Some lock_dir_selection; _ }) ->
      let+ source_lock_dir = select_lock_dir lock_dir_selection in
      Some (source_lock_dir, lock_dir_of_source source_lock_dir)
    | Some (Opam _) -> Memo.return None
  in
  match lock_dir_paths with
  | None -> Memo.return None
  | Some (source_path, lock_dir_path) ->
    let* in_source_tree = Source_tree.find_dir source_path in
    (match in_source_tree with
     | Some _ -> Memo.return (Some (`In_source source_path, `In_build lock_dir_path))
     | None -> Memo.return None)
;;

let get_path_source ctx_name =
  get_paths ctx_name >>| Option.map ~f:(fun (`In_source path, _) -> path)
;;

let get_path ctx_name =
  get_paths ctx_name >>| Option.map ~f:(fun (_, `In_build path) -> path)
;;

let get_workspace_lock_dir ctx =
  let* workspace = Workspace.workspace () in
  let+ path = get_path ctx in
  let open Option.O in
  let* path = path in
  Workspace.find_lock_dir workspace path
;;

let get_with_path ctx =
  let* path =
    get_path ctx
    >>| function
    | Some p -> p
    | None ->
      Code_error.raise
        "No lock dir path for context availabled"
        [ "context", Context_name.to_dyn ctx ]
  in
  let* () = Build_system.build_dir path in
  Load.load path
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
    Ok (path, lock_dir)
;;

let get ctx = get_with_path ctx >>| Result.map ~f:snd
let get_exn ctx = get ctx >>| User_error.ok_exn

let of_dev_tool dev_tool =
  let source_path = dev_tool_source_lock_dir dev_tool in
  Load.load_exn (Path.source source_path)
;;

let of_dev_tool_if_lockdir_exists dev_tool =
  let source_path = dev_tool_source_lock_dir dev_tool in
  let* exists = Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir source_path) in
  if exists
  then
    let+ t = Load.load_exn (Path.source source_path) in
    Some t
  else Memo.return None
;;

let lock_dir_active ctx =
  let open Memo.O in
  if !Clflags.ignore_lock_dir
  then Memo.return false
  else
    let* workspace = Workspace.workspace () in
    match workspace.config.pkg_enabled with
    | Set (_, `Disabled) -> Memo.return false
    | Set (_, `Enabled) | Unset -> get_path ctx >>| Option.is_some
;;

let source_kind (source : Dune_pkg.Source.t) =
  let loc, url = source.url in
  if OpamUrl.is_local url && url.backend = `rsync
  then (
    let path = Path.External.of_string url.path in
    Fs_memo.path_kind (External path)
    >>| function
    | Error (ENOENT, _, _) ->
      User_error.raise
        ~loc
        [ Pp.textf "%s does not exist" (Path.External.to_string_maybe_quoted path) ]
    | Error exn ->
      User_error.raise
        ~loc
        [ Pp.textf "unable to read %s" (Path.External.to_string_maybe_quoted path)
        ; Unix_error.Detailed.pp exn
        ]
    | Ok S_REG -> `Local (`File, path)
    | Ok S_DIR -> `Local (`Directory, path)
    | Ok _kind ->
      User_error.raise
        ~loc
        [ Pp.textf
            "path %s is not a directory or a file"
            (Path.External.to_string_maybe_quoted path)
        ])
  else Memo.return `Fetch
;;
