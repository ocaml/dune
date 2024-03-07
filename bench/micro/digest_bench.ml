open Stdune
module Digest = Dune_digest
module Caml = Stdlib

let create_file size =
  let name = Printf.sprintf "digest-bench-%d" size in
  let out = open_out name in
  for _ = 1 to size do
    output_char out 'X'
  done;
  close_out out;
  at_exit (fun () -> Unix.unlink name);
  name
;;

let%bench_fun ("string" [@indexed len = [ 10; 100; 1_000; 10_000; 1_000_000 ]]) =
  let s = String.make len 'x' in
  fun () -> ignore (Digest.string s)
;;

let%bench_fun ("file" [@indexed len = [ 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]]) =
  let f = Path.of_filename_relative_to_initial_cwd (create_file len) in
  fun () -> ignore (Digest.file f)
;;
