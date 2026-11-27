open Import
open Memo.O

module Backing = struct
  type t =
    | Fs of Path.Outside_build_dir.t
    | Build of Path.Build.t

  let path = function
    | Fs path -> Path.outside_build_dir path
    | Build path -> Path.build path
  ;;

  let repr = Repr.view Path.repr ~to_:path
  let equal a b = Path.equal (path a) (path b)
  let hash t = Path.hash (path t)
  let to_dyn = Repr.to_dyn repr

  let append_local t local =
    match t with
    | Fs path -> Fs (Path.Outside_build_dir.append_local path local)
    | Build path -> Build (Path.Build.append_local path local)
  ;;

  let relative_fname t name =
    match t with
    | Fs path -> Fs (Path.Outside_build_dir.relative_fname path name)
    | Build path -> Build (Path.Build.relative_fname path name)
  ;;

  let file_exists = function
    | Fs path -> Fs_memo.file_exists path
    | Build path -> Build_system.file_exists (Path.build path)
  ;;

  let read_file = function
    | Fs path -> Fs_memo.file_contents path
    | Build path -> Build_system.read_file (Path.build path)
  ;;

  let dir_contents = function
    | Build dir ->
      let+ contents = Build_system.directory_target_contents_opt ~dir in
      Ok contents
    | Fs dir ->
      let+ contents = Fs_memo.dir_contents dir in
      (match contents with
       | Error (Unix.ENOENT, _, _) -> Ok None
       | Error error -> Error error
       | Ok contents ->
         let files, dirs =
           Fs_memo.Dir_contents.to_list contents
           |> List.rev_filter_partition_map ~f:(fun (name, kind) ->
             let name_s = Filename.to_string name in
             match kind with
             | Unix.S_DIR ->
               (match name_s with
                | ".hg" | ".git" | "_darcs" | "_opam" | "_build" | "_esy" -> Skip
                | _ -> Right name)
             | _ ->
               let path =
                 Path.Outside_build_dir.relative_fname dir name |> Path.outside_build_dir
               in
               if String.starts_with ~prefix:".#" name_s || Path.is_broken_symlink path
               then Skip
               else Left name)
         in
         Ok (Some (Filename.Array.Set.of_list files, Filename.Array.Set.of_list dirs)))
  ;;

  let rec find_dir t = function
    | [] -> dir_contents t
    | name :: rest ->
      dir_contents t
      >>= (function
       | Ok (Some (_, dirs)) when Filename.Array.Set.mem dirs name ->
         find_dir (relative_fname t name) rest
       | Ok _ -> Memo.return (Ok None)
       | Error _ as error -> Memo.return error)
  ;;

  let unreadable ~loc t error =
    User_error.raise
      ~loc
      [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted (path t))
      ; Unix_error.Detailed.pp error
      ]
  ;;
end

module Mount = struct
  type t =
    { loc : Loc.t
    ; logical_root : Path.Build.t
    ; backing_root : Backing.t
    }

  let logical_path { logical_root; _ } local = Path.Build.append_local logical_root local
  let backing { backing_root; _ } local = Backing.append_local backing_root local
end

module File = struct
  type t =
    | Workspace of Path.Source.t
    | Mounted of
        { mount : Mount.t
        ; local : Path.Local.t
        }

  let workspace path = Workspace path

  let source_path = function
    | Workspace path -> Path.Source.to_local path
    | Mounted { mount; local } ->
      Mount.logical_path mount local |> Dune_project.dir_in_context
  ;;

  let path = function
    | Workspace path -> Path.source path
    | Mounted { mount; local } -> Mount.logical_path mount local |> Path.build
  ;;

  let backing_path = function
    | Workspace path -> Path.source path
    | Mounted { mount; local } -> Mount.backing mount local |> Backing.path
  ;;

  let as_workspace = function
    | Workspace path -> Some path
    | Mounted _ -> None
  ;;

  let parent_exn = function
    | Workspace file -> Workspace (Path.Source.parent_exn file)
    | Mounted { mount; local } -> Mounted { mount; local = Path.Local.parent_exn local }
  ;;

  let relative t loc path =
    match t with
    | Workspace dir -> Workspace (Path.Source.relative ~error_loc:loc dir path)
    | Mounted { mount; local } ->
      (match Path.Local.relative_result local path with
       | Ok local -> Mounted { mount; local }
       | Error _ ->
         User_error.raise
           ~loc
           [ Pp.textf "Included path %S is outside the mounted source tree." path ])
  ;;

  let exists = function
    | Workspace path -> Fs_memo.file_exists (Path.Outside_build_dir.In_source_dir path)
    | Mounted { mount = { backing_root = Build _; _ } as mount; local } ->
      Backing.file_exists (Mount.backing mount local)
    | Mounted { mount; local } ->
      let file = Mount.backing mount local in
      (* Resolve parent directories through the inventory, not through links. *)
      (match List.rev (Path.Local.explode local) with
       | [] -> Memo.return false
       | name :: dirs ->
         Backing.find_dir mount.backing_root (List.rev dirs)
         >>= (function
          | Ok (Some (files, _)) when Filename.Array.Set.mem files name ->
            Backing.file_exists file
          | Error _ | Ok _ -> Memo.return false))
  ;;

  let read t =
    let* exists = exists t in
    if not exists
    then Memo.return None
    else
      let+ contents =
        match t with
        | Workspace path ->
          Fs_memo.file_contents (Path.Outside_build_dir.In_source_dir path)
        | Mounted { mount; local } -> Backing.read_file (Mount.backing mount local)
      in
      Some contents
  ;;

  let equal a b =
    Path.equal (path a) (path b) && Path.equal (backing_path a) (backing_path b)
  ;;

  let diagnostic_name t = Path.to_string (path t)

  let with_lexbuf_from_file t ~f =
    read t
    >>| function
    | Some contents -> f (Lexbuf.from_string contents ~fname:(diagnostic_name t))
    | None ->
      User_error.raise
        ~loc:(Loc.in_file (backing_path t))
        [ Pp.textf "File %s cannot be read." (diagnostic_name t) ]
  ;;
end

module Dir = struct
  include File

  let mounted ~loc ~logical_root ~backing_root =
    Mounted { mount = { Mount.loc; logical_root; backing_root }; local = Path.Local.root }
  ;;

  let basename = function
    | Workspace path -> Path.Source.basename path
    | Mounted { mount; local } -> Mount.logical_path mount local |> Path.Build.basename
  ;;

  let file t filename =
    match t with
    | Workspace path -> Path.Source.relative_fname path filename |> File.workspace
    | Mounted { mount; local } ->
      Mounted { mount; local = Path.Local.relative_fname local filename }
  ;;

  let contents = function
    | Workspace path ->
      Dir_contents.of_source_path path
      >>| (function
       | Ok dir -> dir
       | Error _ -> Dir_contents.empty)
    | Mounted { mount; local } ->
      let backing = Mount.backing mount local in
      Backing.dir_contents backing
      >>| (function
       | Ok None -> Dir_contents.empty
       | Ok (Some (files, dirs)) -> Dir_contents.of_names ~files ~dirs
       | Error error -> Backing.unreadable ~loc:mount.loc backing error)
  ;;
end
