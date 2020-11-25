open! Stdune
open! Dune_engine

(* usual value for PATH_MAX *)
let max_path_length = 4096

let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let encode buf e =
  Printf.bprintf buf "(Sys.opaque_identity %S)"
    (Artifact_substitution.encode ~min_len:max_path_length e)

let helpers = "Dune_site.Private_.Helpers"

let plugins = "Dune_site_plugins.Private_.Plugins"

let sourceroot_code buf =
  pr buf "let sourceroot = %s.sourceroot %a" helpers encode
    (Configpath Sourceroot)

let relocatable_code buf =
  pr buf "let relocatable = Lazy.force %s.relocatable" helpers

let sites_code sctx buf (loc, pkg) =
  let package =
    match Package.Name.Map.find (Super_context.packages sctx) pkg with
    | Some p -> p
    | None ->
      User_error.raise ~loc [ Pp.text "dune_site used outside a package" ]
  in
  let package_name = Package.name package in
  (* Parse the replacement format described in [artifact_substitution.ml]. *)
  Section.Site.Map.iteri package.sites ~f:(fun name section ->
      pr buf "    let %s = %s.site"
        (String.uncapitalize_ascii (Section.Site.to_string name))
        helpers;
      pr buf "      ~package:%S" (Package.Name.to_string package_name);
      pr buf "      ~section:Dune_section.%s"
        (String.capitalize_ascii (Section.to_string section));
      pr buf "      ~suffix:%S" (Section.Site.to_string name);
      pr buf "      ~encoded:%a" encode (Location (section, package_name)))

let plugins_code sctx buf pkg sites =
  let package =
    match Package.Name.Map.find (Super_context.packages sctx) pkg with
    | Some p -> p
    | None -> assert false
  in
  let pkg = Package.Name.to_string pkg in
  (* Parse the replacement format described in [artifact_substitution.ml]. *)
  List.iter sites ~f:(fun (loc, ssite) ->
      let site = Section.Site.to_string ssite in
      if not (Section.Site.Map.mem package.sites ssite) then
        User_error.raise ~loc
          [ Pp.textf "Package %s doesn't define a site %s" pkg site ];
      pr buf "    module %s : %s.S = %s.Make(struct let paths = Sites.%s end)"
        (String.capitalize site) plugins plugins site)

let setup_rules sctx ~dir (def : Dune_file.Generate_sites_module.t) =
  let buf = Buffer.create 1024 in
  if def.sourceroot then sourceroot_code buf;
  if def.relocatable then relocatable_code buf;
  let sites =
    List.sort_uniq
      ~compare:(fun (_, pkga) (_, pkgb) -> Package.Name.compare pkga pkgb)
      (def.sites @ List.map ~f:(fun (loc, (pkg, _)) -> (loc, pkg)) def.plugins)
  in
  if List.is_non_empty sites then (
    pr buf "module Sites = struct";
    List.iter sites ~f:(sites_code sctx buf);
    pr buf "end"
  );
  let plugins = Package.Name.Map.of_list_multi (List.map ~f:snd def.plugins) in
  if not (Package.Name.Map.is_empty plugins) then (
    pr buf "module Plugins = struct";
    Package.Name.Map.iteri plugins ~f:(plugins_code sctx buf);
    pr buf "end"
  );
  let impl = Buffer.contents buf in
  let module_ = Module_name.to_string def.module_ ^ ".ml" in
  let file = Path.Build.relative dir module_ in
  Super_context.add_rule sctx ~dir (Build.write_file file impl);
  module_
