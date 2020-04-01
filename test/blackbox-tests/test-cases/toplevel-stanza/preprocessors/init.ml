open Core
let () =
  Printf.printf "Foo.x = %s\n%!" (([%sexp_of: int] Foo.x) |> Sexp.to_string);
  exit 0
