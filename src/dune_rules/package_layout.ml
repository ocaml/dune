open Import
open Action_builder.O

let materialize context packages =
  let prefix = Install_layout.root context packages |> Path.build in
  let* dependencies =
    Action_builder.List.map (Package.Name.Set.to_list packages) ~f:(fun name ->
      let* opaque = Action_builder.of_memo (Opam_package.find context name) in
      match opaque with
      | Some opaque -> Opam_package.dependency opaque
      | None ->
        let+ packages = Action_builder.of_memo (Dune_load.packages ()) in
        let package = Package.Name.Map.find_exn packages name in
        let version =
          Package.version package
          |> Option.value ~default:(Package_version.of_string "dev")
        in
        let paths =
          Opam_build.Paths.of_root name ~root:prefix ~relative:Path.relative
          |> fun paths -> Opam_build.Paths.with_prefix paths prefix
        in
        { Opam_build.Dependency.paths
        ; variables = Opam_build.variables ~name ~version ~dev:false
        ; files = Section.Map.empty
        ; exported_env = []
        ; export_paths = paths
        })
  in
  Install_layout.materialize context dependencies
;;
