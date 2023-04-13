open Import

include Dep.Set.Source_tree (struct
  module Map_reduce =
    Source_tree.Dir.Make_map_reduce (Memo) (Monoid.Union (Dep.Set))

  type 'a result = 'a Memo.t

  let union_all dir ~f =
    let prefix_with, dir = Path.extract_build_context_dir_exn dir in
    let open Memo.O in
    Source_tree.find_dir dir >>= function
    | None -> Memo.return Dep.Set.empty
    | Some dir ->
      Map_reduce.map_reduce dir ~traverse:Sub_dirs.Status.Set.all ~f:(fun dir ->
          let path =
            Path.append_source prefix_with @@ Source_tree.Dir.path dir
          in
          let files = Source_tree.Dir.files dir in
          Memo.return @@ f ~path ~files)
end)
