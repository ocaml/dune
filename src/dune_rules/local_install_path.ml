open! Stdune
open! Import

let dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative Dpath.Build.install_dir context

let lib_root ~context = Path.Build.relative (dir ~context) "lib"

let bin_dir ~context = Path.Build.relative (dir ~context) "bin"

let man_dir ~context = Path.Build.relative (dir ~context) "bin"

let lib_dir ~context ~package =
  Path.Build.relative (lib_root ~context) (Package.Name.to_string package)
