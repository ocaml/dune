open Import

let dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative Dpath.Build.install_dir context
;;

let lib_root ~context = Path.Build.relative (dir ~context) "lib"
let bin_dir ~context = Path.Build.relative (dir ~context) "bin"
let man_dir ~context = Path.Build.relative (dir ~context) "man"

let lib_dir ~context ~package =
  Path.Build.relative (lib_root ~context) (Package_name.to_string package)
;;

let of_path path =
  match Dune_engine.Dpath.analyse_dir (Path.build path) with
  | Build
      ( Install (With_context (name, _))
      | Regular (With_context (name, _))
      | Anonymous_action (With_context (name, _)) ) -> Some name
  | _ -> None
;;
