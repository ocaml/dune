open Import
open Memo.O

module Map_reduce =
  Source_tree.Rules.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product (Monoid.Union (Path.Set)) (Monoid.Union (Path.Set)))

let files_with_filter path ~filter =
  let* source_dir =
    match Path.as_in_source_tree path with
    | Some dir -> Source_tree.find_dir dir >>| Option.map ~f:Source_tree.Rules.Dir.source
    | None -> Dune_load.source_tree_dir ~dir:(Path.as_in_build_dir_exn path)
  in
  match source_dir with
  | None -> Memo.return (Dep.Set.empty, Path.Set.empty)
  | Some source_dir ->
    let root = Source_tree.Rules.Dir.source_path source_dir in
    let+ files, empty_directories =
      Map_reduce.map_reduce
        source_dir
        ~traverse:Source_dir_status.Set.all
        ~trace_event_name:"Source deps"
        ~f:(fun dir ->
          let relative =
            Source_path.descendant (Source_tree.Rules.Dir.source_path dir) ~of_:root
            |> Option.value_exn
          in
          let path = Path.append_local path relative in
          let files =
            Source_tree.Rules.Dir.filenames dir
            |> Filename.Array.Set.to_list
            |> Path.Set.of_list_map ~f:(fun fn -> Path.relative_fname path fn)
            |> Path.Set.filter ~f:filter
          in
          let empty_directories =
            if Path.Set.is_empty files then Path.Set.singleton path else Path.Set.empty
          in
          Memo.return (files, empty_directories))
    in
    Dep.Set.of_source_files ~files ~empty_directories, files
;;

let files = files_with_filter ~filter:(Fun.const true)
