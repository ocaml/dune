open Import

type 'path t =
  { lib : 'path
  ; lib_root : 'path
  ; libexec : 'path
  ; libexec_root : 'path
  ; bin : 'path
  ; sbin : 'path
  ; toplevel : 'path
  ; share : 'path
  ; share_root : 'path
  ; etc : 'path
  ; doc : 'path
  ; stublibs : 'path
  ; man : 'path
  }

let map
  { lib
  ; lib_root
  ; libexec
  ; libexec_root
  ; bin
  ; sbin
  ; toplevel
  ; share
  ; share_root
  ; etc
  ; doc
  ; stublibs
  ; man
  }
  ~f
  =
  { lib = f lib
  ; lib_root = f lib_root
  ; libexec = f libexec
  ; libexec_root = f libexec_root
  ; bin = f bin
  ; sbin = f sbin
  ; toplevel = f toplevel
  ; share = f share
  ; share_root = f share_root
  ; etc = f etc
  ; doc = f doc
  ; stublibs = f stublibs
  ; man = f man
  }
;;

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
  | Misc -> Code_error.raise "Install.Paths.get" []
;;

let make ~relative ~package ~(roots : _ Roots.t) =
  let f = get_field ~relative ~package ~roots in
  { lib_root = f Lib_root
  ; libexec_root = f Libexec_root
  ; share_root = f Share_root
  ; bin = f Bin
  ; sbin = f Sbin
  ; man = f Man
  ; toplevel = f Toplevel
  ; stublibs = f Stublibs
  ; lib = f Lib
  ; libexec = f Libexec
  ; share = f Share
  ; etc = f Etc
  ; doc = f Doc
  }
;;

let get t (section : Section.t) =
  match section with
  | Lib -> t.lib
  | Lib_root -> t.lib_root
  | Libexec -> t.libexec
  | Libexec_root -> t.libexec_root
  | Bin -> t.bin
  | Sbin -> t.sbin
  | Toplevel -> t.toplevel
  | Share -> t.share
  | Share_root -> t.share_root
  | Etc -> t.etc
  | Doc -> t.doc
  | Stublibs -> t.stublibs
  | Man -> t.man
  | Misc -> Code_error.raise "Install.Paths.get" []
;;

let get_local_location context section package_name =
  (* check that we get the good path *)
  let install_dir = Context.dir ~context in
  let install_dir = Path.build install_dir in
  let roots = Roots.opam_from_prefix install_dir ~relative:Path.relative in
  get_field ~relative:Path.relative ~package:package_name ~roots section
;;
