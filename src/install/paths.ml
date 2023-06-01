open Import

type t =
  { lib : Path.t
  ; lib_root : Path.t
  ; libexec : Path.t
  ; libexec_root : Path.t
  ; bin : Path.t
  ; sbin : Path.t
  ; toplevel : Path.t
  ; share : Path.t
  ; share_root : Path.t
  ; etc : Path.t
  ; doc : Path.t
  ; stublibs : Path.t
  ; man : Path.t
  }

let make ~package ~(roots : Path.t Roots.t) =
  let package = Package_name.to_string package in
  { lib_root = roots.lib_root
  ; libexec_root = roots.libexec_root
  ; share_root = roots.share_root
  ; bin = roots.bin
  ; sbin = roots.sbin
  ; man = roots.man
  ; toplevel = Path.relative roots.lib_root "toplevel"
  ; stublibs = Path.relative roots.lib_root "stublibs"
  ; lib = Path.relative roots.lib_root package
  ; libexec = Path.relative roots.libexec_root package
  ; share = Path.relative roots.share_root package
  ; etc = Path.relative roots.etc_root package
  ; doc = Path.relative roots.doc_root package
  }

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

let get_local_location context section package_name =
  (* check that we get the good path *)
  let install_dir = Context.dir ~context in
  let install_dir = Path.build install_dir in
  let roots = Roots.opam_from_prefix install_dir in
  let paths = make ~package:package_name ~roots in
  get paths section
