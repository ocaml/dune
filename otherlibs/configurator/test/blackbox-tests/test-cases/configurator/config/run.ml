module Configurator = Configurator.V1

let () =
  begin match Sys.getenv "INSIDE_DUNE" with
  | exception Not_found -> failwith "INSIDE_DUNE is not passed"
  | "1" -> print_endline "INSIDE_DUNE is from an old dune"
  | dir -> print_endline "INSIDE_DUNE is present";
    if Sys.file_exists (Filename.concat dir ".dune/configurator") then
      print_endline ".dune/configurator file present"
    else
      print_endline ".dune/configurator file not present"
  end;
  Configurator.main ~name:"config" (fun t ->
    match Configurator.ocaml_config_var t "version" with
    | None -> failwith "version is absent"
    | Some _ -> print_endline "version is present"
  )
