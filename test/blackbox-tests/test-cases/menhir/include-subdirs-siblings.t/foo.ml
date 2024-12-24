let () =
  assert (Lang.Parser.expr (fun _ -> Lang.Parser.EOF) (Lexing.from_string "") = Lang.Ast.Unit)
