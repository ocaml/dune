open Import

module Clflags = struct
  type t =
    | No_restriction
    | Restrict of
        { names : Package.Name.Set.t
        ; command_line_option : string
        }

  let equal a b =
    match a, b with
    | No_restriction, No_restriction -> true
    | ( Restrict { names = a_names; command_line_option = a_command_line_options }
      , Restrict { names = b_names; command_line_option = b_command_line_options } ) ->
      Package.Name.Set.equal a_names b_names
      && String.equal a_command_line_options b_command_line_options
    | _, _ -> false
  ;;

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

type t = Package.t list Package.Name.Map.t option

let enumerate = function
  | None -> `All
  | Some m -> `Set (Package.Name.Set.of_keys m)
;;

let mem_all = Option.is_none

let mem t name =
  match t with
  | None -> true
  | Some map -> Package.Name.Map.mem map name
;;

let mask packages : t =
  let enabled, has_disabled =
    Package.Name.Map.foldi
      packages
      ~init:(Package.Name.Map.empty, false)
      ~f:(fun name packages acc ->
        List.fold_left
          ~init:acc
          ~f:(fun (enabled, has_disabled) (pkg, status) ->
            if status.Package.enabled
            then Package.Name.Map.add_multi enabled name pkg, has_disabled
            else enabled, true)
          packages)
  in
  match
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
      Package.Name.Map.filter_map packages ~f:(fun packages ->
        match
          List.filter_map packages ~f:(fun (pkg, { enabled; vendored }) ->
            let name = Package.name pkg in
            let included = Package.Name.Set.mem names name in
            Option.some_if (enabled && (vendored || included)) pkg)
        with
        | [] -> None
        | l -> Some l)
      |> Option.some
  with
  | None -> if has_disabled then Some enabled else None
  | Some p ->
    Some
      (Package.Name.Map.merge p enabled ~f:(fun _ masked enabled ->
         match masked, enabled with
         | Some x, Some _ -> Some x
         | _, _ -> None))
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
