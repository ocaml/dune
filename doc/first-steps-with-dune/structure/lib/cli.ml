let rec eval = function Ast.Int n -> n | Add (a, b) -> eval a + eval b

let info = Cmdliner.Cmd.info "calc"

let eval_lb lb =
  let e = Parser.main Lexer.token lb in
  Printf.printf "%d\n" (eval e)

let repl () =
  while true do
    Printf.printf ">> %!";
    let lb = Lexing.from_channel Stdlib.stdin in
    eval_lb lb
  done

let term =
  let open Cmdliner.Term.Syntax in
  let+ expr_opt =
    let open Cmdliner.Arg in
    value & opt (some string) None & info [ "e" ]
  in
  match expr_opt with
  | Some s -> eval_lb (Lexing.from_string s)
  | None -> repl ()

let cmd = Cmdliner.Cmd.v info term
let main () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
