module Configurator = Configurator.V1

let () =
  begin match Sys.getenv "INSIDE_DUNE" with
  | exception Not_found -> failwith "INSIDE_DUNE is not passed"
  | "1" -> print_endline "INSIDE_DUNE is from an old dune"
  | dir -> print_endline "INSIDE_DUNE is present";
    let config_path = ".dune/configurator.v2" in
    Printf.printf "%s file is %s\n" config_path
      (if Sys.file_exists (Filename.concat dir config_path) then
         "present"
       else
         "not present")
  end;
  Configurator.main ~name:"config" (fun t ->
    match Configurator.ocaml_config_var t "version" with
    | None -> failwith "version is absent"
    | Some _ -> print_endline "version is present"
  )
