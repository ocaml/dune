module C = Configurator.V1

let static = Option.map bool_of_string (Sys.getenv_opt "STATIC_TEST__")

let () =
  C.main ~name:"config_test" (fun t ->
    let pkg_config =
      match C.Pkg_config.get ?static t with
      | None -> assert false
      | Some p -> p
    in
    let query package = ignore (C.Pkg_config.query pkg_config ~package) in
    query "dummy-pkg";
  )
