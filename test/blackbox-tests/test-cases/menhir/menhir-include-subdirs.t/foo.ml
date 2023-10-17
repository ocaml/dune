let () =
  Bar.Baz.unit (fun _ -> Bar.Baz.EOF) (Lexing.from_string "")
