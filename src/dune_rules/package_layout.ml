open Import
open Action_builder.O

let materialize_ordered context names =
  let packages = Package.Name.Set.of_list names in
  let prefix = Install_layout.root context packages |> Path.build in
  let* dependencies =
    Action_builder.List.map names ~f:(fun name ->
      let* local_package =
        Action_builder.of_memo
          (let open Memo.O in
           let* candidate = Pkg_sources.find_candidate context name in
           match candidate with
           | Some _ -> Memo.return None
           | None ->
             let+ packages = Dune_load.packages_for_context context in
             Package.Name.Map.find packages name)
      in
      match local_package with
      | None ->
        let* dependency = Action_builder.of_memo (Pkg_rules.dependency context name) in
        (match dependency with
         | Some dependency -> dependency
         | None ->
           User_error.raise
             [ Pp.textf "Package %s does not exist" (Package.Name.to_string name) ])
      | Some package ->
        let* opaque = Action_builder.of_memo (Opam_package.find context name) in
        (match opaque with
         | Some opaque -> Opam_package.dependency opaque
         | None ->
           let version =
             Package.version package
             |> Option.value ~default:(Package_version.of_string "dev")
           in
           let paths =
             Opam_build.Paths.of_root name ~root:prefix ~relative:Path.relative
             |> fun paths -> Opam_build.Paths.with_prefix paths prefix
           in
           Action_builder.return
             { Opam_build.Dependency.paths
             ; variables = Opam_build.variables ~name ~version ~dev:false
             ; files = Section.Map.empty
             ; exported_env = []
             ; export_paths = paths
             }))
  in
  Install_layout.materialize context dependencies
;;

let materialize context packages =
  materialize_ordered context (Package.Name.Set.to_list packages)
;;

let materialize_dependencies context owner =
  let* direct, ordered =
    Action_builder.of_memo
      (let open Memo.O in
       let* workspace = Dune_load.packages ()
       and* candidates = Pkg_sources.candidates context in
       let candidates =
         Package.Name.Map.of_list_map_exn candidates ~f:(fun candidate ->
           Pkg_sources.Candidate.name candidate, candidate)
       in
       let versions =
         Package.Name.Map.union_exn
           (Package.Name.Map.map workspace ~f:(fun package ->
              Package.version package
              |> Option.value ~default:(Package_version.of_string "dev")))
           (Package.Name.Map.map candidates ~f:(fun candidate ->
              (Pkg_sources.Candidate.lock_pkg candidate).info.version))
       in
       let* polled = Pkg_sys_vars.solver_env in
       let* active = Lock_dir.lock_dir_active context in
       let+ recorded =
         if not active
         then Memo.return Dune_pkg.Solver_env.empty
         else
           let+ lock_dir = Lock_dir.get_exn context in
           Dune_pkg.Solver_stats.Expanded_variable_bindings.to_solver_env
             lock_dir.expanded_solver_variable_bindings
       in
       let solver_env =
         Dune_pkg.Solver_env.extend
           (Dune_pkg.Solver_env.extend Dune_pkg.Solver_env.with_defaults polled)
           recorded
       in
       let provided name = Package.Name.Map.mem versions name in
       let system_dune name =
         Package.Name.equal name Dune_pkg.Dune_dep.name && not (provided name)
       in
       let dependencies name =
         match Package.Name.Map.find workspace name with
         | Some package ->
           let required =
             match
               Dune_pkg.Package_universe.resolve_local_dependencies
                 (Dune_pkg.Local_package.of_package package)
                 ~env:solver_env
                 ~with_test:false
                 ~packages:versions
             with
             | Ok required -> required
             | Error hints ->
               User_error.raise
                 ~loc:(Package.loc package)
                 (Pp.textf
                    "The dependencies of package %S could not be satisfied:"
                    (Package.Name.to_string name)
                  :: hints)
           in
           let optional =
             List.filter_map
               (Package.depopts package)
               ~f:(fun (dep : Package_dependency.t) ->
                 Option.some_if (provided dep.name) dep.name)
           in
           required @ optional
         | None ->
           (match Package.Name.Map.find candidates name with
            | None ->
              User_error.raise
                [ Pp.textf "Package %s does not exist" (Package.Name.to_string name) ]
            | Some candidate ->
              let pkg = Pkg_sources.Candidate.lock_pkg candidate in
              Dune_pkg.Lock_dir.Conditional_choice.choose_for_platform
                pkg.depends
                ~platform:polled
              |> Option.value ~default:[]
              |> List.filter_map ~f:(fun (dep : Dune_pkg.Lock_dir.Dependency.t) ->
                Option.some_if (not (system_dune dep.name)) dep.name))
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
