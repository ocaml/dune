open! Stdune
open! Import

let dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative Dpath.Build.install_dir context

let lib_root ~context = Path.Build.relative (dir ~context) "lib"

let bin_dir ~context = Path.Build.relative (dir ~context) "bin"

let man_dir ~context = Path.Build.relative (dir ~context) "man"

let lib_dir ~context ~package =
  Path.Build.relative (lib_root ~context) (Package.Name.to_string package)

(* let extend_vars ~root = *)
(*   let lib = _ in *)
(*   let man = _ in *)
(*   [ ("CAML_LD_LIBRARY_PATH", Path.Build.relative lib "stublibs") *)
(*   ; ("OCAMLPATH", lib) *)
(*   ; ("OCAMLTOP_INCLUDE_PATH", Path.Build.relative lib "toplevel") *)
(*   ; ("OCAMLFIND_IGNORE_DUPS_IN", lib) *)
(*   ; ("MANPATH", man) *)
(*   ] *)
