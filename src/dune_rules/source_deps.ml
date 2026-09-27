open Import
open Memo.O

module Map_reduce =
  Source_tree.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product (Monoid.Union (Path.Set)) (Monoid.Union (Path.Set)))

module Mounted_map_reduce =
  Source_tree.Rules.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product (Monoid.Union (Path.Set)) (Monoid.Union (Path.Set)))

let deps_of_files (files, empty_directories) =
  Dep.Set.of_source_files ~files ~empty_directories, files
;;

let workspace_files_with_filter dir ~filter =
  let prefix_with, dir =
    match (dir : Path.t) with
    | In_source_tree dir -> Path.root, dir
    | otherwise -> Path.extract_build_context_dir_exn otherwise
  in
  Source_tree.find_dir dir
  >>= function
  | None -> Memo.return (Dep.Set.empty, Path.Set.empty)
  | Some dir ->
    Map_reduce.map_reduce
      dir
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Source deps"
      ~f:(fun dir ->
        let path = Path.append_source prefix_with @@ Source_tree.Dir.path dir in
        let files =
          Source_tree.Dir.filenames dir
          |> Filename.Array.Set.to_list
          |> Path.Set.of_list_map ~f:(fun fn -> Path.relative_fname path fn)
          |> Path.Set.filter ~f:filter
        in
        let empty_directories =
          if Path.Set.is_empty files then Path.Set.singleton path else Path.Set.empty
        in
        Memo.return (files, empty_directories))
    >>| deps_of_files
;;

let mounted_files_with_filter dir ~filter =
  let* loaded_project = Dune_load.find_loaded_project ~dir in
  Loaded_project.source_tree_dir loaded_project dir
  >>= function
  | None -> Memo.return (Dep.Set.empty, Path.Set.empty)
  | Some source_dir ->
    Mounted_map_reduce.map_reduce
      source_dir
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Mounted source deps"
      ~f:(fun source_dir ->
        let path = Source_tree.Rules.Dir.path source_dir in
        let files =
          Source_tree.Rules.Dir.filenames source_dir
          |> Filename.Array.Set.to_list
          |> Path.Set.of_list_map ~f:(fun fn -> Path.relative_fname path fn)
          |> Path.Set.filter ~f:filter
        in
        let empty_directories =
          if Path.Set.is_empty files then Path.Set.singleton path else Path.Set.empty
        in
        Memo.return (files, empty_directories))
    >>| deps_of_files
;;

let files_with_filter path ~filter =
  match (path : Path.t) with
  | In_build_dir dir ->
    (match Pkg_sources.package_of_artifact_path dir with
     | Some _ -> mounted_files_with_filter dir ~filter
     | None -> workspace_files_with_filter path ~filter)
  | In_source_tree _ | External _ -> workspace_files_with_filter path ~filter
;;

let files = files_with_filter ~filter:(Fun.const true)
