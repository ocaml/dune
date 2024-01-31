open Import

module Root_field = struct
  type t =
    | Lib_root
    | Libexec_root
    | Share_root
    | Etc_root
    | Doc_root

  let get (roots : _ Roots.t) = function
    | Lib_root -> roots.lib_root
    | Libexec_root -> roots.libexec_root
    | Share_root -> roots.share_root
    | Etc_root -> roots.etc_root
    | Doc_root -> roots.doc_root
  ;;
end

type 'path t =
  { package : Package_name.t
  ; roots : 'path Roots.t
  ; subdir : Root_field.t -> string -> 'path
  }

let map { roots; package; subdir } ~f =
  let roots = Roots.map ~f roots in
  let subdir field s = f (subdir field s) in
  { roots; package; subdir }
;;

let make ~relative ~package ~roots =
  let subdir field s = relative (Root_field.get roots field) s in
  { package; roots; subdir }
;;

let get { roots; subdir; package } (section : Section.t) =
  let package = Package_name.to_string package in
  match section with
  | Lib -> subdir Lib_root package
  | Lib_root -> roots.lib_root
  | Libexec -> subdir Libexec_root package
  | Libexec_root -> roots.libexec_root
  | Bin -> roots.bin
  | Sbin -> roots.sbin
  | Toplevel -> subdir Lib_root "toplevel"
  | Share -> subdir Share_root package
  | Share_root -> roots.share_root
  | Etc -> subdir Etc_root package
  | Doc -> subdir Doc_root package
  | Stublibs -> subdir Lib_root "stublibs"
  | Man -> roots.man
  | Misc -> Code_error.raise "Install.Paths.get" []
;;

let get_local_location context section package_name =
  (* check that we get the good path *)
  let install_dir = Context.dir ~context in
  let install_dir = Path.build install_dir in
  let roots = Roots.opam_from_prefix install_dir ~relative:Path.relative in
  let paths = make ~relative:Path.relative ~package:package_name ~roots in
  get paths section
;;
