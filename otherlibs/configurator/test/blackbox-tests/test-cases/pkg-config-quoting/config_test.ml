module C = Configurator.V1


let () =
  C.main ~name:"config_test" (fun t ->
    let pkg_config =
      match C.Pkg_config.get t with
      | None -> assert false
      | Some p -> p
    in
    let query package = ignore (C.Pkg_config.query pkg_config ~package) in
    query "gtk+-quartz-3.0";
    query "gtk+-quartz-3.0 >= 3.18";
    query "gtksourceview-3.0 >= 3.18"
  )
