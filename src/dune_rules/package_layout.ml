open Import
open Action_builder.O

let materialize_ordered context names =
  let packages = Package.Name.Set.of_list names in
  let prefix = Install_layout.root context packages |> Path.build in
  let* dependencies =
    Action_builder.List.map names ~f:(fun name ->
      let* locked = Action_builder.of_memo (Pkg_rules.find_locked context name) in
      match locked with
      | Some (candidate, `Opam) -> Pkg_rules.dependency context candidate
      | Some (candidate, `Dune _) ->
        let { Dune_pkg.Lock_dir.Pkg.info; exported_env; _ } =
          Pkg_rules.Candidate.lock_pkg candidate
        in
        let paths =
          Pkg_rules.candidate_paths candidate |> Pkg_rules.Paths.map_path ~f:Path.build
        in
        Action_builder.return
          (Pkg_rules.Dependency.native
             paths
             ~prefix
             ~variables:(Dune_pkg.Lock_dir.Pkg_info.variables info)
             ~exported_env)
      | None ->
        let* package = Action_builder.of_memo (Partitions.find_package context name) in
        (match package with
         | None ->
           User_error.raise
             [ Pp.textf "Package %s does not exist" (Package.Name.to_string name) ]
         | Some package ->
           let version =
             Package.version package |> Option.value ~default:Dune_pkg.Package_version.dev
           in
           let paths =
             Pkg_rules.Paths.of_root
               name
               ~root:prefix
               ~source_dir:"source"
               ~install_dir:"target"
               ~relative:Path.relative
           in
           let variables =
             Package_variable_name.Map.of_list_exn
               [ Package_variable_name.name, OpamVariable.S (Package.Name.to_string name)
               ; ( Package_variable_name.version
                 , S (Dune_pkg.Package_version.to_string version) )
               ; Package_variable_name.dev, B false
               ]
           in
           Action_builder.return
             (Pkg_rules.Dependency.native paths ~prefix ~variables ~exported_env:[])))
  in
  Pkg_rules.Layout.materialize context dependencies
;;

let materialize context packages =
  materialize_ordered context (Package.Name.Set.to_list packages)
;;

let materialize_dependencies context owner =
  let* direct, ordered =
    Action_builder.of_memo
      (let open Memo.O in
       let+ workspace = Dune_load.packages ()
       and+ locked = Pkg_rules.dependencies context
       and+ platform = Lock_dir.Sys_vars.solver_env
       and+ lock_dir = Lock_dir.get_exn context in
       let universe =
         Dune_pkg.Package_universe.create_unvalidated
           ~platform
           (Package.Name.Map.map workspace ~f:Dune_pkg.Local_package.of_package)
           lock_dir
       in
       let dependencies name =
         if Package.Name.Map.mem workspace name
         then
           Dune_pkg.Package_universe.selected_dependencies_of_local_package
             universe
             name
             ~with_test:false
         else (
           match Package.Name.Map.find locked name with
           | None ->
             User_error.raise
               [ Pp.textf "Package %s does not exist" (Package.Name.to_string name) ]
           | Some dependencies -> dependencies)
       in
       let direct = dependencies owner in
       let module Closure = Top_closure.Make (Package.Name.Set) (Monad.Id) in
       match Closure.top_closure ~key:Fun.id ~deps:dependencies direct with
       | Ok ordered -> Package.Name.Set.of_list direct, ordered
       | Error cycle ->
         User_error.raise
           [ Pp.text "Dependency cycle between packages:"
           ; Pp.chain cycle ~f:(fun name -> Pp.verbatim (Package.Name.to_string name))
           ])
  in
  let+ layout = materialize_ordered context ordered in
  direct, layout
;;
