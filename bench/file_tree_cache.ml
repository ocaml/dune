(* Benchmark File_tree.t's internal cache *)

open Stdune
open Dune

let deep_path = "a1/a2/a3/a4/a5/a6/a7/a8/a9/10"

let (ft, path) =
  let tmp = Path.External.of_string (Filename.get_temp_dir_name ()) in
  Path.External.mkdir_p (Path.External.relative tmp deep_path);
  Path.set_root tmp;
  Path.set_build_dir (Path.Kind.of_string "_build");
  let ft = (Dune_load.load ()).file_tree in
  let path = Path.of_string deep_path in
  (ft, path)

let%bench "File_tree.find_dir" =
  ignore (Sys.opaque_identity (File_tree.find_dir ft path)
          : File_tree.Dir.t option)
