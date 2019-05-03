(* Benchmark File_tree.t's internal cache *)

open Stdune
open Dune

let deep_path = "a1/a2/a3/a4/a5/a6/a7/a8/a9/10"

let setup = lazy (
  let tmp = Path.External.of_string (Filename.get_temp_dir_name ()) in
  Path.External.mkdir_p (Path.External.relative tmp deep_path);
  Path.set_root tmp;
  Path.set_build_dir (Path.Kind.of_string_exn "_build");
  let ft = (Dune_load.load ~ancestor_vcs:None ()).file_tree in
  let path = Path.Source.of_string_exn deep_path in
  at_exit (fun () -> Sys.remove "./dune-project");
  (ft, path))

let%bench_fun "File_tree.find_dir" =
  let (ft, path) = Lazy.force setup in
  fun () ->
    ignore (Sys.opaque_identity (File_tree.find_dir ft path)
            : File_tree.Dir.t option)
