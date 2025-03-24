  $ calc -e '1+2'
  3

  $ calc -e '1+'
  parse error near character 2

  $ calc -e '1+2.5'
  3.5

  $ calc -e '1+pi'
  4.14159

  $ calc -e '1+2*3'
  7

  $ calc -e '4/2'
  2

  $ calc -e 'sin (pi / 6)'
  0.5

  $ calc -e 'log10(123456)'
  5.09151

  $ calc --debug-ast -e '2 * sin (pi / 2)'
  2
  [debug] (Ast.Op (Ast.Mul, (Ast.Int 2),
             (Ast.Call ("sin",
                (Ast.Op (Ast.Div, (Ast.Ident "pi"), (Ast.Int 2)))))
             ))
