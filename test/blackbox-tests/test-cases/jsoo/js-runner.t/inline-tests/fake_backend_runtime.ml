
let l = ref []
let register_test f = l := f :: !l

let run_all () =
  print_endline "Before runtest";
  List.iter (fun f -> f ()) !l;
  print_endline "After runtest";

