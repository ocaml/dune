open Import
open Memo.O

let dune_sites_env ~default_ocamlpath ~stdlib =
  [ Dune_site_private.dune_ocaml_stdlib_env_var, Path.to_absolute_filename stdlib
  ; ( Dune_site_private.dune_ocaml_hardcoded_env_var
    , List.map ~f:Path.to_absolute_filename default_ocamlpath
      |> String.concat ~sep:(Char.escaped Findlib_config.ocamlpath_sep) )
  ; ( Dune_site_private.dune_sourceroot_env_var
    , Path.to_absolute_filename (Path.source Path.Source.root) )
  ]
  |> String.Map.of_list_exn
  |> Env.of_string_map
;;

let add_packages_env context ~base stanzas packages =
  let* base =
    let* context = Context.DB.get context in
    let* ocaml = Context.ocaml context in
    let+ default_ocamlpath = Context.default_ocamlpath context in
    Env.extend_env
      base
      (dune_sites_env ~default_ocamlpath ~stdlib:ocaml.lib_config.stdlib_dir)
  in
  let+ env_dune_dir_locations =
    let init =
      match Stdune.Env.get base Dune_site_private.dune_dir_locations_env_var with
      | None -> []
      | Some var ->
        (match Dune_site_private.decode_dune_dir_locations var with
         | Some s -> s
         | None ->
           User_error.raise
             [ Pp.textf
                 "Invalid env var %s=%S"
                 Dune_site_private.dune_dir_locations_env_var
                 var
             ])
    in
    let+ package_sections =
      (* Add the section of the site mentioned in stanzas (it could be a site
         of an external package) *)
      let add_in_package_section m pkg section =
        Package.Name.Map.update m pkg ~f:(function
          | None -> Some (Section.Set.singleton section)
          | Some s -> Some (Section.Set.add s section))
      in
      let+ package_sections =
        let* package_db = Package_db.create context in
        Dune_file.Memo_fold.fold_static_stanzas
          stanzas
          ~init:Package.Name.Map.empty
          ~f:(fun _ stanza acc ->
            let add_in_package_sites pkg_name site loc =
              Package_db.find_package package_db pkg_name
              >>| function
              | None ->
                (* Really ugly to suppress errors like this. Instead,
                   executables that rely on sites should declare that
                   in their dependencies *)
                acc
              | Some pkg ->
                let section =
                  Package_db.section_of_any_package_site pkg pkg_name loc site
                in
                add_in_package_section acc pkg_name section
            in
            match Stanza.repr stanza with
            | Install_conf.T { section = _loc, Site { pkg; site; loc }; _ } ->
              add_in_package_sites pkg site loc
            | Plugin.T { site = loc, (pkg, site); _ } -> add_in_package_sites pkg site loc
            | _ -> Memo.return acc)
      in
      (* Add the site of the local package: it should only useful for making
         sure that at least one location is given to the site of local package
         because if the site is used it should already be in
         [packages_sections] *)
      Package.Name.Map.foldi
        packages
        ~init:package_sections
        ~f:(fun package_name (package : Package.t) acc ->
          Package.sites package
          |> Site.Map.fold ~init:acc ~f:(fun section acc ->
            add_in_package_section acc package_name section))
    in
    let roots =
      Install.Context.dir ~context
      |> Path.build
      |> Install.Roots.opam_from_prefix ~relative:Path.relative
    in
    Package.Name.Map.foldi ~init package_sections ~f:(fun package_name sections init ->
      let paths =
        Install.Paths.make ~relative:Path.relative ~package:package_name ~roots
      in
      Section.Set.fold sections ~init ~f:(fun section acc ->
        let package = Package.Name.to_string package_name in
        let dir = Path.to_absolute_filename (Install.Paths.get paths section) in
        { Dune_site_private.package; dir; section } :: acc))
  in
  if List.is_empty env_dune_dir_locations
  then base
  else
    Stdune.Env.add
      base
      ~var:Dune_site_private.dune_dir_locations_env_var
      ~value:(Dune_site_private.encode_dune_dir_locations env_dune_dir_locations)
;;
