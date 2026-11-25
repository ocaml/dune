open Import
open Memo.O

let dune_dir_locations_env_var =
  Env.Var.of_string Dune_site_private.dune_dir_locations_env_var
;;

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

let sites_by_context =
  let add_site m pkg site section =
    Package.Name.Map.update m pkg ~f:(fun sections ->
      let sections = Option.value sections ~default:Section.Map.empty in
      let sites =
        Option.value (Section.Map.find sections section) ~default:Site.Set.empty
      in
      Some (Section.Map.set sections section (Site.Set.add sites site)))
  in
  Memo.create
    "site-package-sites"
    ~input:(module Context_name)
    (fun context ->
       let* stanzas = Dune_load.dune_files context
       and* packages = Dune_load.packages () in
       let+ referenced_sites =
         (* A stanza may refer to a site in an external package. *)
         let* package_db = Package_db.create context in
         Dune_file.Memo_fold.fold_static_stanzas
           stanzas
           ~init:Package.Name.Map.empty
           ~f:(fun _ stanza acc ->
             match Stanza.repr stanza with
             | Install_conf.T { section = _, Site { pkg; site; loc }; _ }
             | Plugin.T { site = loc, (pkg, site); _ } ->
               Package_db.find_package package_db pkg
               >>| (function
                | None -> acc
                | Some package ->
                  let section =
                    Package_db.section_of_any_package_site package pkg loc site
                  in
                  add_site acc pkg site section)
             | _ -> Memo.return acc)
       in
       (* Include local sites even when no stanza names them. *)
       Package.Name.Map.foldi packages ~init:referenced_sites ~f:(fun pkg package acc ->
         Package.sites package
         |> Site.Map.foldi ~init:acc ~f:(fun site section acc ->
           add_site acc pkg site section)))
  |> Memo.exec
;;

let set_locations env = function
  | [] -> env
  | entries ->
    Env.add
      env
      ~var:dune_dir_locations_env_var
      ~value:(Dune_site_private.encode_dune_dir_locations entries)
;;

let entry_parents ~root entries =
  let rec add_parents dir parents =
    if Path.Build.equal dir root || Path.Build.Set.mem parents dir
    then parents
    else add_parents (Path.Build.parent_exn dir) (Path.Build.Set.add parents dir)
  in
  (* Directory entries themselves are handled by [covered_by_directory]. *)
  Path.Build.Map.foldi
    (Path.Build.Map.remove entries root)
    ~init:Path.Build.Set.empty
    ~f:(fun dst _ parents -> add_parents (Path.Build.parent_exn dst) parents)
;;

type layout =
  | Staging
  | Scoped of Path.t Install.Entry.Expanded.t Path.Build.Map.t

let locations ~root layout package_sites =
  let populated =
    match layout with
    | Staging -> fun _ _ -> true
    | Scoped entries ->
      let parents = entry_parents ~root entries in
      let rec covered_by_directory dir =
        match Path.Build.Map.find entries dir with
        | Some { Install.Entry.kind = Directory; _ } -> true
        | _ ->
          (not (Path.Build.equal dir root))
          && covered_by_directory (Path.Build.parent_exn dir)
      in
      fun dir sites ->
        Site.Set.exists sites ~f:(fun site ->
          let site_dir = Path.Build.relative dir (Site.to_string site) in
          Path.Build.Set.mem parents site_dir || covered_by_directory site_dir)
  in
  let roots = Install.Roots.opam_from_prefix root ~relative:Path.Build.relative in
  Package.Name.Map.foldi package_sites ~init:[] ~f:(fun package_name sections acc ->
    let paths =
      Install.Paths.make ~relative:Path.Build.relative ~package:package_name ~roots
    in
    Section.Map.foldi sections ~init:acc ~f:(fun section sites acc ->
      let dir = Install.Paths.get paths section in
      if populated dir sites
      then (
        let package = Package.Name.to_string package_name in
        let dir = Path.to_absolute_filename (Path.build dir) in
        { Dune_site_private.package; dir; section } :: acc)
      else acc))
;;

let install_env context ~root ~entries =
  let site_env =
    Memo.lazy_ ~name:"install-layout-site-env" (fun () ->
      let+ sites = sites_by_context context in
      if Package.Name.Map.is_empty sites
      then Env.empty
      else set_locations Env.empty (locations ~root (Scoped entries) sites))
  in
  let open Action_builder.O in
  let* env = Action_builder.of_memo (Memo.Lazy.force site_env) in
  if Env.is_empty env
  then Action_builder.return env
  else
    let+ () = Action_builder.env_var dune_dir_locations_env_var in
    env
;;

let add_packages_env context ~base =
  let* base =
    let+ ocaml = Context.ocaml context
    and+ default_ocamlpath = Context.default_ocamlpath context in
    Env.extend_env
      base
      (dune_sites_env ~default_ocamlpath ~stdlib:ocaml.lib_config.stdlib_dir)
  in
  let init =
    match Env.get base dune_dir_locations_env_var with
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
  let context = Context.name context in
  let+ sites = sites_by_context context in
  let root = Install.Context.dir ~context in
  set_locations base (locations ~root Staging sites @ init)
;;
