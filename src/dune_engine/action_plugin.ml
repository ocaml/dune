open Import
module DAP = Dune_action_plugin.Private.Protocol

let to_dune_dep_set =
  let of_DAP_dep ~loc ~working_dir : DAP.Dependency.t -> Dep.t =
    let to_dune_path = Path.relative working_dir in
    function
    | File fn -> Dep.file (to_dune_path fn)
    | Directory dir ->
      let dir = to_dune_path dir in
      let selector = File_selector.of_glob ~dir Glob.universal in
      Dep.file_selector selector
    | Glob { path; glob } ->
      let dir = to_dune_path path in
      let glob = Glob.of_string_exn loc glob in
      let selector = File_selector.of_glob ~dir glob in
      Dep.file_selector selector
  in
  fun set ~loc ~working_dir ->
    DAP.Dependency.Set.to_list_map set ~f:(of_DAP_dep ~loc ~working_dir)
    |> Dep.Set.of_list
;;

type done_or_more_deps =
  | Done
  (* This code assumes that there can be at most one 'dynamic-run' within single
     action. [DAP.Dependency.t] stores relative paths so name clash would be
     possible if multiple 'dynamic-run' would be executed in different
     subdirectories that contains targets having the same name. *)
  | Need_more_deps of (DAP.Dependency.Set.t * Dep.Set.t)

let done_or_more_deps_union x y =
  match x, y with
  | Done, Done -> Done
  | Done, Need_more_deps x | Need_more_deps x, Done -> Need_more_deps x
  | Need_more_deps (deps1, dyn_deps1), Need_more_deps (deps2, dyn_deps2) ->
    Need_more_deps
      (DAP.Dependency.Set.union deps1 deps2, Dep.Set.union dyn_deps1 dyn_deps2)
;;
