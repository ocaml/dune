let () =
  Baz.unit (fun _ -> Baz.EOF) (Lexing.from_string "");
  Format.printf "foo@."
