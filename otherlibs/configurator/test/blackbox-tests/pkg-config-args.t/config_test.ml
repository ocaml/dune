module C = Configurator.V1


let () =
  C.main ~name:"config_test" (fun t ->
    let pkg_config =
      match C.Pkg_config.get t with
      | None -> assert false
      | Some p -> p
    in
    let query package = ignore (C.Pkg_config.query pkg_config ~package) in
    query "dummy-pkg";
    (match C.Pkg_config.query_variable pkg_config ~package:"dummy-pkg" ~variable:"prefix" with
     | Some "value-for-prefix" -> ()
     | Some value -> failwith ("unexpected pkg-config variable value: " ^ value)
     | None -> failwith "pkg-config variable query failed")
  )
