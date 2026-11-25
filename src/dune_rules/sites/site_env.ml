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
    Package.Name.Map.update m pkg ~f:(fun sites ->
      Some (Site.Map.set (Option.value sites ~default:Site.Map.empty) site section))
  in
  Memo.create
    "site-package-sites"
    ~input:(module Context_name)
    (fun context ->
       let* stanzas = Dune_load.dune_files context
       and* packages = Dune_load.packages () in
       let declared_sites =
         Package.Name.Map.filter_map packages ~f:(fun package ->
           let sites = Package.sites package in
           Option.some_if (not (Site.Map.is_empty sites)) sites)
       in
       let+ sites =
         (* A stanza may also refer to a site in an external package. *)
         let* package_db = Package_db.create context in
         Dune_file.Memo_fold.fold_static_stanzas
           stanzas
           ~init:declared_sites
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
       Package.Name.Map.map sites ~f:(fun sites ->
         Site.Map.foldi sites ~init:Section.Map.empty ~f:(fun site section acc ->
           Section.Map.Multi.cons acc section site)))
  |> Memo.exec
;;

type location =
  { package : string
  ; section : Section.t
  ; dir : Path.Build.t
  ; sites : Site.t list
  }

let locations ~root package_sites =
  let roots = Install.Roots.opam_from_prefix root ~relative:Path.Build.relative in
  Package.Name.Map.foldi package_sites ~init:[] ~f:(fun pkg sections acc ->
    let paths = Install.Paths.make ~relative:Path.Build.relative ~package:pkg ~roots in
    let package = Package.Name.to_string pkg in
    Section.Map.foldi sections ~init:acc ~f:(fun section sites acc ->
      { package; section; dir = Install.Paths.get paths section; sites } :: acc))
;;

let set_locations env ~fallbacks locations =
  let entries =
    List.fold_right locations ~init:fallbacks ~f:(fun { package; section; dir; _ } acc ->
      let dir = Path.to_absolute_filename (Path.build dir) in
      { Dune_site_private.package; section; dir } :: acc)
  in
  match entries with
  | [] -> env
  | _ ->
    Env.add
      env
      ~var:dune_dir_locations_env_var
      ~value:(Dune_site_private.encode_dune_dir_locations entries)
;;

let populated_locations ~root entries = function
  | [] -> []
  | locations ->
    let parents =
      let rec add dir parents =
        if Path.Build.equal dir root || Path.Build.Set.mem parents dir
        then parents
        else add (Path.Build.parent_exn dir) (Path.Build.Set.add parents dir)
      in
      (* Directory entries themselves are handled by [covered_by_directory]. *)
      Path.Build.Map.foldi
        (Path.Build.Map.remove entries root)
        ~init:Path.Build.Set.empty
        ~f:(fun dst _ parents -> add (Path.Build.parent_exn dst) parents)
    in
    let rec covered_by_directory dir =
      match Path.Build.Map.find entries dir with
      | Some { Install.Entry.kind = Install.Entry.Expanded.Directory; _ } -> true
      | _ ->
        (not (Path.Build.equal dir root))
        && covered_by_directory (Path.Build.parent_exn dir)
    in
    List.filter locations ~f:(fun { dir; sites; _ } ->
      List.exists sites ~f:(fun site ->
        let site_dir = Path.Build.relative dir (Site.to_string site) in
        Path.Build.Set.mem parents site_dir || covered_by_directory site_dir))
;;

let install_env context ~root ~entries =
  let site_env =
    Memo.lazy_ ~name:"install-layout-site-env" (fun () ->
      let+ sites = sites_by_context context in
      locations ~root sites
      |> populated_locations ~root entries
      |> set_locations Env.empty ~fallbacks:[])
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
  set_locations base ~fallbacks:init (locations ~root sites)
;;
