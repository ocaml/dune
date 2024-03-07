open Import

module Map_reduce =
  Source_tree.Dir.Make_map_reduce
    (Memo)
    (Monoid.Product (Monoid.Union (Path.Set)) (Monoid.Union (Path.Set)))

let files dir =
  let prefix_with, dir = Path.extract_build_context_dir_exn dir in
  let open Memo.O in
  Source_tree.find_dir dir
  >>= function
  | None -> Memo.return (Dep.Set.empty, Path.Set.empty)
  | Some dir ->
    let+ files, empty_directories =
      Map_reduce.map_reduce dir ~traverse:Source_dir_status.Set.all ~f:(fun dir ->
        let path = Path.append_source prefix_with @@ Source_tree.Dir.path dir in
        let files =
          Source_tree.Dir.filenames dir
          |> String.Set.to_list
          |> Path.Set.of_list_map ~f:(fun fn -> Path.relative path fn)
        in
        let empty_directories =
          if Path.Set.is_empty files then Path.Set.singleton path else Path.Set.empty
        in
        Memo.return (files, empty_directories))
    in
    Dep.Set.of_source_files ~files ~empty_directories, files
;;
