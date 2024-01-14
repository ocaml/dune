open Import
open Memo.O
open Dune_pkg
include Dune_pkg.Lock_dir

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

let get_path ctx =
  let+ workspace = Workspace.workspace () in
  match
    List.find_map workspace.contexts ~f:(fun ctx' ->
      match Context_name.equal (Workspace.Context.name ctx') ctx with
      | false -> None
      | true -> Some ctx')
  with
  | None -> Some default_path
  | Some (Default { lock_dir; _ }) -> Some (Option.value lock_dir ~default:default_path)
  | Some (Opam _) -> None
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
