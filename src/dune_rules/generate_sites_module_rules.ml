open Import

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

let is_ocaml_keywords = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false

let sanitize_site_name name =
  let rec aux name i =
    if i < 0 then i else if name.[i] = '_' then aux name (i - 1) else i
  in
  let name = String.uncapitalize_ascii (Section.Site.to_string name) in
  let last = String.length name - 1 in
  let i = aux name last in
  let s = if i <> last then String.sub name ~pos:0 ~len:(i + 1) else name in
  if is_ocaml_keywords s then name ^ "_" else name

let sites_code packages buf (loc, pkg) =
  let package =
    match Package.Name.Map.find packages pkg with
    | Some p -> p
    | None -> User_error.raise ~loc [ Pp.text "Unknown package" ]
  in
  let package_name = Package.name package in
  Section.Site.Map.iteri package.sites ~f:(fun name section ->
      pr buf "    let %s = %s.site" (sanitize_site_name name) helpers;
      pr buf "      ~package:%S" (Package.Name.to_string package_name);
      pr buf "      ~section:Dune_section.%s"
        (String.capitalize_ascii (Section.to_string section));
      pr buf "      ~suffix:%S" (Section.Site.to_string name);
      pr buf "      ~encoded:%a" encode (Location (section, package_name)))

let plugins_code packages buf pkg sites =
  let package : Package.t =
    match Package.Name.Map.find packages pkg with
    | Some p -> p
    | None -> assert false
  in
  let pkg = Package.Name.to_string pkg in
  (* Parse the replacement format described in [artifact_substitution.ml]. *)
  List.iter sites ~f:(fun (loc, ssite) ->
      let site = sanitize_site_name ssite in
      if not (Section.Site.Map.mem package.sites ssite) then
        User_error.raise ~loc
          [ Pp.textf "Package %s doesn't define a site %s" pkg site ];
      pr buf "    module %s : %s.S = %s.Make(struct let paths = Sites.%s end)"
        (String.capitalize site) plugins plugins site)

let setup_rules sctx ~dir (def : Dune_file.Generate_sites_module.t) =
  let open Memo.O in
  let* packages = Only_packages.get () in
  let impl () =
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
      List.iter sites ~f:(sites_code packages buf);
      pr buf "end");
    let plugins =
      Package.Name.Map.of_list_multi (List.map ~f:snd def.plugins)
    in
    if not (Package.Name.Map.is_empty plugins) then (
      pr buf "module Plugins = struct";
      Package.Name.Map.iteri plugins ~f:(plugins_code packages buf);
      pr buf "end");
    Buffer.contents buf
  in
  let module_ = Module_name.to_string def.module_ ^ ".ml" in
  let file = Path.Build.relative dir module_ in
  let+ () =
    Super_context.add_rule sctx ~dir
      (Action_builder.write_file_dyn file (Action_builder.delayed impl))
  in
  module_
