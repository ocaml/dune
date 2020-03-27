open Core
let () =
  (* Using the ppx - test hangs *)
  Printf.printf "Foo.x = %s\n%!" (([%sexp_of: int] Foo.x) |> Sexp.to_string);
  (* Printf.printf "Foo.x = %d\n%!" Foo.x; *)
  exit 0
