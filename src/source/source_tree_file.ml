open Import
open Memo.O

module Mounted_layer = struct
  type t =
    | Directory of
        { logical_root : Path.Build.t
        ; backing_root : Path.Build.t
        }
    | File of
        { logical : Path.Build.t
        ; backing : Path.Build.t
        }
    | Contents of
        { logical : Path.Build.t
        ; contents : string
        ; executable : bool
        ; dependencies : Path.t list
        }
    | Delete of { logical : Path.Build.t }

  let directory ~logical_root ~backing_root = Directory { logical_root; backing_root }
  let file ~logical ~backing = File { logical; backing }

  let contents ~logical ~contents ~executable ~dependencies =
    Contents { logical; contents; executable; dependencies }
  ;;

  let delete ~logical = Delete { logical }

  let equal a b =
    match a, b with
    | ( Directory { logical_root; backing_root }
      , Directory { logical_root = logical_root'; backing_root = backing_root' } ) ->
      Path.Build.equal logical_root logical_root'
      && Path.Build.equal backing_root backing_root'
    | File { logical; backing }, File { logical = logical'; backing = backing' } ->
      Path.Build.equal logical logical' && Path.Build.equal backing backing'
    | ( Contents { logical; contents; executable; dependencies }
      , Contents
          { logical = logical'
          ; contents = contents'
          ; executable = executable'
          ; dependencies = dependencies'
          } ) ->
      Path.Build.equal logical logical'
      && String.equal contents contents'
      && Bool.equal executable executable'
      && List.equal Path.equal dependencies dependencies'
    | Delete { logical }, Delete { logical = logical' } ->
      Path.Build.equal logical logical'
    | Directory _, (File _ | Contents _ | Delete _)
    | File _, (Directory _ | Contents _ | Delete _)
    | Contents _, (Directory _ | File _ | Delete _)
    | Delete _, (Directory _ | File _ | Contents _) -> false
  ;;

  let hash = function
    | Directory { logical_root; backing_root } ->
      Tuple.T3.hash
        Int.hash
        Path.Build.hash
        Path.Build.hash
        (0, logical_root, backing_root)
    | File { logical; backing } ->
      Tuple.T3.hash Int.hash Path.Build.hash Path.Build.hash (1, logical, backing)
    | Contents { logical; contents; executable; dependencies } ->
      Tuple.T3.hash
        Int.hash
        Path.Build.hash
        (Tuple.T3.hash String.hash Bool.hash (List.hash Path.hash))
        (2, logical, (contents, executable, dependencies))
    | Delete { logical } -> Tuple.T2.hash Int.hash Path.Build.hash (3, logical)
  ;;

  let to_dyn = function
    | Directory { logical_root; backing_root } ->
      Dyn.variant
        "Directory"
        [ Dyn.record
            [ "logical_root", Path.Build.to_dyn logical_root
            ; "backing_root", Path.Build.to_dyn backing_root
            ]
        ]
    | File { logical; backing } ->
      Dyn.variant
        "File"
        [ Dyn.record
            [ "logical", Path.Build.to_dyn logical; "backing", Path.Build.to_dyn backing ]
        ]
    | Contents { logical; contents = _; executable; dependencies } ->
      Dyn.variant
        "Contents"
        [ Dyn.record
            [ "logical", Path.Build.to_dyn logical
            ; "executable", Dyn.bool executable
            ; "dependencies", Dyn.list Path.to_dyn dependencies
            ]
        ]
    | Delete { logical } -> Dyn.variant "Delete" [ Path.Build.to_dyn logical ]
  ;;

  type resolution =
    | Candidate of
        { path : Path.Build.t
        ; blockers : Path.Build.t list
        }
    | Inline of
        { contents : string
        ; executable : bool
        ; dependencies : Path.t list
        }
    | Blocked
    | Unrelated

  let proper_ancestors path =
    let rec loop parent = function
      | [] | [ _ ] -> []
      | component :: rest ->
        let parent = Path.Local.relative_fname parent component in
        parent :: loop parent rest
    in
    loop Path.Local.root (Path.Local.explode path)
  ;;

  let resolve_file t logical =
    match t with
    | Directory { logical_root; backing_root } ->
      (match Path.drop_prefix (Path.build logical) ~prefix:(Path.build logical_root) with
       | None -> Unrelated
       | Some local ->
         Candidate
           { path = Path.Build.append_local backing_root local
           ; blockers =
               List.map (proper_ancestors local) ~f:(Path.Build.append_local backing_root)
           })
    | File { logical = layer_logical; backing } ->
      if Path.Build.equal logical layer_logical
      then Candidate { path = backing; blockers = [] }
      else if Path.is_descendant (Path.build logical) ~of_:(Path.build layer_logical)
      then Blocked
      else Unrelated
    | Contents { logical = layer_logical; contents; executable; dependencies } ->
      if Path.Build.equal logical layer_logical
      then Inline { contents; executable; dependencies }
      else if Path.is_descendant (Path.build logical) ~of_:(Path.build layer_logical)
      then Blocked
      else Unrelated
    | Delete { logical = layer_logical } ->
      if
        Path.Build.equal logical layer_logical
        || Path.is_descendant (Path.build logical) ~of_:(Path.build layer_logical)
      then Blocked
      else Unrelated
  ;;
end

module File = struct
  type mounted =
    { logical : Path.Build.t
    ; layers : Mounted_layer.t list
    }

  type t =
    | Workspace of Path.Source.t
    | Mounted of mounted

  let workspace path = Workspace path
  let mounted ~logical ~layers = Mounted { logical; layers }

  let source_path = function
    | Workspace path -> Source_path.workspace path
    | Mounted { logical; _ } -> Source_path.build logical
  ;;

  let path = function
    | Workspace path -> Path.source path
    | Mounted { logical; _ } -> Path.build logical
  ;;

  type contents =
    { contents : string
    ; executable : bool
    ; dependencies : Path.t list
    }

  type materialization =
    | Copy of Path.t
    | Write of contents

  let find_materialization { logical; layers } =
    let rec loop = function
      | [] -> Memo.return None
      | layer :: layers ->
        (match Mounted_layer.resolve_file layer logical with
         | Blocked -> Memo.return None
         | Unrelated -> loop layers
         | Inline { contents; executable; dependencies } ->
           Memo.return (Some (Write { contents; executable; dependencies }))
         | Candidate { path; blockers } ->
           let* exists = Build_system.file_exists (Path.build path) in
           if exists
           then Memo.return (Some (Copy (Path.build path)))
           else
             let* blocked =
               Memo.List.exists blockers ~f:(fun path ->
                 Build_system.file_exists (Path.build path))
             in
             if blocked then Memo.return None else loop layers)
    in
    loop (List.rev layers)
  ;;

  let materialization = function
    | Workspace path -> Memo.return (Some (Copy (Path.source path)))
    | Mounted mounted -> find_materialization mounted
  ;;

  let backing_path_opt t =
    materialization t
    >>| function
    | Some (Copy path) -> Some path
    | Some (Write _) | None -> None
  ;;

  let backing_path t =
    materialization t
    >>| function
    | Some (Copy path) -> path
    | Some (Write _) ->
      Code_error.raise
        "Generated mounted source file has no immutable backing path"
        [ "file", Source_path.to_dyn (source_path t) ]
    | None ->
      Code_error.raise
        "Mounted source file has no backing input"
        [ "file", Source_path.to_dyn (source_path t) ]
  ;;

  let as_workspace = function
    | Workspace path -> Some path
    | Mounted _ -> None
  ;;

  let relative t loc path =
    match t with
    | Workspace file ->
      Path.Source.relative ~error_loc:loc (Path.Source.parent_exn file) path |> workspace
    | Mounted { logical; layers } ->
      mounted
        ~logical:(Path.Build.relative ~error_loc:loc (Path.Build.parent_exn logical) path)
        ~layers
  ;;

  let executable path =
    match Path.Untracked.stat path with
    | Error _ -> false
    | Ok { st_perm; _ } ->
      Permissions.test_any Permissions.execute (Permissions.Mode.of_int st_perm)
  ;;

  let read_with_perm = function
    | Workspace path ->
      let outside = Path.Outside_build_dir.In_source_dir path in
      let* exists = Fs_memo.file_exists outside in
      if not exists
      then Memo.return None
      else
        let+ contents = Fs_memo.file_contents outside in
        let path = Path.source path in
        Some { contents; executable = executable path; dependencies = [ path ] }
    | Mounted _ as t ->
      materialization t
      >>= (function
       | None -> Memo.return None
       | Some (Write contents) -> Memo.return (Some contents)
       | Some (Copy path) ->
         let+ contents = Build_system.read_file path in
         Some { contents; executable = executable path; dependencies = [ path ] })
  ;;

  let read t = read_with_perm t >>| Option.map ~f:(fun { contents; _ } -> contents)

  let equal a b =
    match a, b with
    | Workspace a, Workspace b -> Path.Source.equal a b
    | Mounted a, Mounted b ->
      Path.Build.equal a.logical b.logical
      && List.equal Mounted_layer.equal a.layers b.layers
    | Workspace _, Mounted _ | Mounted _, Workspace _ -> false
  ;;

  let diagnostic_name = function
    | Workspace path -> Path.Source.to_string path
    | Mounted { logical; _ } -> Path.Build.to_string logical
  ;;

  let to_dyn = function
    | Workspace path -> Dyn.variant "Workspace" [ Path.Source.to_dyn path ]
    | Mounted { logical; layers } ->
      Dyn.variant
        "Mounted"
        [ Dyn.record
            [ "logical", Path.Build.to_dyn logical
            ; "layers", Dyn.list Mounted_layer.to_dyn layers
            ]
        ]
  ;;
end

module Dir = struct
  type mounted =
    { logical : Path.Build.t
    ; backing : Path.Build.t
    ; layers : Mounted_layer.t list
    }

  type t =
    | Workspace of Path.Source.t
    | Mounted of mounted

  let workspace path = Workspace path
  let mounted ~logical ~backing ~layers = Mounted { logical; backing; layers }

  let source_path = function
    | Workspace path -> Source_path.workspace path
    | Mounted { logical; _ } -> Source_path.build logical
  ;;

  let path = function
    | Workspace path -> Path.source path
    | Mounted { logical; _ } -> Path.build logical
  ;;

  let backing_path = function
    | Workspace path -> Path.source path
    | Mounted { backing; _ } -> Path.build backing
  ;;

  let basename = function
    | Workspace path -> Path.Source.basename path
    | Mounted { logical; _ } -> Path.Build.basename logical
  ;;

  let file t filename =
    match t with
    | Workspace path -> Path.Source.relative_fname path filename |> File.workspace
    | Mounted { logical; layers; _ } ->
      File.mounted ~logical:(Path.Build.relative_fname logical filename) ~layers
  ;;
end
