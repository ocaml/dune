module Configurator = Configurator.V1

let () =
  Configurator.main ~name:"config" (fun t ->
    match Configurator.ocaml_config_var t "version" with
    | None -> failwith "version is absent"
    | Some _ -> print_endline "version is present"
  )
