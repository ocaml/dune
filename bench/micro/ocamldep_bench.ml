open Stdune
open Base

let make_output dep_count =
  let buffer = Stdlib.Buffer.create (16 + (dep_count * 12)) in
  Stdlib.Buffer.add_string buffer "src/lib/foo.ml:";
  for i = 1 to dep_count do
    Stdlib.Buffer.add_char buffer ' ';
    Stdlib.Printf.bprintf buffer "Dep%04d" i
  done;
  Stdlib.Buffer.add_char buffer '\n';
  Stdlib.Buffer.contents buffer
;;

let%bench_fun
    ("parse_output"
     [@params
       dep_count
       = [ "0 deps", 0
         ; "10 deps", 10
         ; "100 deps", 100
         ; "1_000 deps", 1_000
         ; "5_000 deps", 5_000
         ]])
  =
  let file = Path.of_string "/tmp/foo.ml" in
  let output = make_output dep_count in
  fun () -> ignore (Dune_rules.Ocamldep.parse_output ~file output)
;;
