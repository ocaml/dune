let rec eval = function Ast.Int n -> n | Add (a, b) -> eval a + eval b

let () =
  while true do
    Printf.printf ">> %!";
    let lb = Lexing.from_channel Stdlib.stdin in
    let e = Parser.main Lexer.token lb in
    Printf.printf "%d\n" (eval e)
  done
