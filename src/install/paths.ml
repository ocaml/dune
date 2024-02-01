open Import

(** Invariant: map contains [Section.all] except [Misc] *)
type 'path t = 'path Section.Map.t

let map t ~f = Section.Map.map t ~f

let get_field ~relative ~package ~(roots : _ Roots.t) (section : Section.t) =
  let package = Package_name.to_string package in
  match section with
  | Lib -> relative roots.lib_root package
  | Lib_root -> roots.lib_root
  | Libexec -> relative roots.libexec_root package
  | Libexec_root -> roots.libexec_root
  | Bin -> roots.bin
  | Sbin -> roots.sbin
  | Toplevel -> relative roots.lib_root "toplevel"
  | Share -> relative roots.share_root package
  | Share_root -> roots.share_root
  | Etc -> relative roots.etc_root package
  | Doc -> relative roots.doc_root package
  | Stublibs -> relative roots.lib_root "stublibs"
  | Man -> roots.man
  | Misc -> Code_error.raise "Install.Paths.get_field" []
;;

let make ~relative ~package ~roots =
  let f = get_field ~relative ~package ~roots in
  let mapped_sections = Section.Set.remove Section.all Misc in
  Section.Set.to_map mapped_sections ~f
;;

let get t section =
  match Section.Map.find t section with
  | Some path -> path
  | None -> Code_error.raise "Install.Paths.get" []
;;

let get_local_location context section package_name =
  (* check that we get the good path *)
  let install_dir = Context.dir ~context in
  let install_dir = Path.build install_dir in
  let roots = Roots.opam_from_prefix install_dir ~relative:Path.relative in
  get_field ~relative:Path.relative ~package:package_name ~roots section
;;
