open Import

let print_lib (lib:Dune_file.Library.t) =
  Option.iter lib.public
    ~f:(fun pub ->
      Format.printf "Public library: %a\n" Package.Name.pp pub.package.name;
      let deps =
        lib.buildable.libraries
        |> List.concat_map ~f:Dune_file.Lib_dep.to_lib_names
        |> List.map ~f:Lib_name.to_string
        |> String.concat ~sep:", "
      in
      Format.printf "Dependencies: %s\n" deps;
      Option.iter ~f:(Format.printf "Synopsis: %s\n") lib.synopsis
    )

let iter_stanzas_in_project project dune_files ~f =
  List.iter dune_files
    ~f:(fun (dune_file:Dune_load.Dune_file.t) ->
      if Dune_project.equal project dune_file.project then
        List.iter dune_file.stanzas ~f)

let print_stanza = function
  | Dune_file.Library lib -> print_lib lib
  | _ -> ()

let describe project dune_files =
  Printf.printf "Project name: %s\n"
    (Dune_project.Name.to_string_hum (Dune_project.name project));
  iter_stanzas_in_project project dune_files ~f:print_stanza
