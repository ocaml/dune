open Import

module Clflags = struct
  type t =
    | No_restriction
    | Restrict of
        { names : Package.Name.Set.t
        ; command_line_option : string
        }

  let to_dyn = function
    | No_restriction -> Dyn.Variant ("No_restriction", [])
    | Restrict { names; command_line_option } ->
      Variant
        ( "Restrict"
        , [ Record
              [ "names", Package.Name.Set.to_dyn names
              ; "command_line_option", String command_line_option
              ]
          ] )
  ;;

  let t = Fdecl.create to_dyn
  let set x = Fdecl.set t x
  let t () = Fdecl.get t
end

type t = Package.t Package.Name.Map.t option

let mask packages ~vendored : t =
  match Clflags.t () with
  | No_restriction -> None
  | Restrict { names; command_line_option } ->
    Package.Name.Set.iter names ~f:(fun pkg_name ->
      if not (Package.Name.Map.mem packages pkg_name)
      then (
        let pkg_name = Package.Name.to_string pkg_name in
        User_error.raise
          [ Pp.textf
              "I don't know about package %s (passed through %s)"
              pkg_name
              command_line_option
          ]
          ~hints:
            (User_message.did_you_mean
               pkg_name
               ~candidates:
                 (Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string))));
    Package.Name.Map.filter_map packages ~f:(fun pkg ->
      let name = Package.name pkg in
      let vendored = Package.Name.Set.mem vendored name in
      let included = Package.Name.Set.mem names name in
      Option.some_if (vendored || included) pkg)
    |> Option.some
;;

let filter_packages (t : t) packages =
  match t with
  | None -> packages
  | Some mask ->
    Package.Name.Map.merge packages mask ~f:(fun _ p mask ->
      match p, mask with
      | Some _, Some _ -> p
      | _ -> None)
;;

let filter_packages_in_project ~vendored project =
  match Clflags.t () with
  | No_restriction -> project
  | Restrict { names; command_line_option } ->
    (match vendored with
     | false -> Dune_project.filter_packages project ~f:(Package.Name.Set.mem names)
     | true ->
       let () =
         Dune_project.packages project
         |> Package.Name.Set.of_keys
         |> Package.Name.Set.inter names
         |> Package.Name.Set.iter ~f:(fun name ->
           User_error.raise
             [ Pp.textf
                 "Package %s is vendored and so will never be masked. It is redundant to \
                  pass it to %s."
                 (Package.Name.to_string name)
                 command_line_option
             ])
       in
       project)
;;
