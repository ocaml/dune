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
end

module Load = Make_load (struct
    include Memo

    let readdir_with_kinds path =
      Fs_memo.dir_contents (In_source_dir path)
      >>| function
      | Error _ ->
        (* CR-rgrinberg: add some proper message here *)
        User_error.raise [ Pp.text "" ]
      | Ok content -> Fs_cache.Dir_contents.to_list content
    ;;

    let with_lexbuf_from_file path ~f =
      Fs_memo.with_lexbuf_from_file (In_source_dir path) ~f
    ;;

    let stats_kind p =
      Fs_memo.path_stat (In_source_dir p)
      >>| Stdune.Result.map ~f:(fun { Fs_cache.Reduced_stats.st_kind; _ } -> st_kind)
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

let get_path ctx =
  let* workspace = Workspace.workspace () in
  match
    List.find_map workspace.contexts ~f:(fun ctx' ->
      match Context_name.equal (Workspace.Context.name ctx') ctx with
      | false -> None
      | true -> Some ctx')
  with
  | None -> Memo.return (Some default_path)
  | Some (Default { lock_dir = Some lock_dir_selection; _ }) ->
    select_lock_dir lock_dir_selection >>| Option.some
  | Some (Default { lock_dir = None; _ }) -> Memo.return (Some default_path)
  | Some (Opam _) -> Memo.return None
;;

let get_workspace_lock_dir ctx =
  let* workspace = Workspace.workspace () in
  let+ path = get_path ctx >>| Option.value_exn in
  Workspace.find_lock_dir workspace path
;;

let get (ctx : Context_name.t) : t Memo.t =
  let* lock_dir = get_path ctx >>| Option.value_exn >>= Load.load in
  let+ workspace_lock_dir = get_workspace_lock_dir ctx in
  (match workspace_lock_dir with
   | None -> ()
   | Some workspace_lock_dir ->
     Solver_stats.Expanded_variable_bindings.validate_against_solver_env
       lock_dir.expanded_solver_variable_bindings
       (workspace_lock_dir.solver_env |> Option.value ~default:Solver_env.empty));
  lock_dir
;;

let lock_dir_active ctx =
  if !Clflags.ignore_lock_dir
  then Memo.return false
  else
    get_path ctx
    >>= function
    | None -> Memo.return false
    | Some path -> Fs_memo.dir_exists (In_source_dir path)
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
