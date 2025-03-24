type value = VInt of int | VFloat of float

let value_to_string = function
  | VInt n -> string_of_int n
  | VFloat f -> Printf.sprintf "%.6g" f

let eval_number_op f_int f_float va vb =
  match (va, vb) with
  | VInt na, VInt nb -> VInt (f_int na nb)
  | VFloat fa, VFloat fb -> VFloat (f_float fa fb)
  | VInt na, VFloat fb -> VFloat (f_float (float_of_int na) fb)
  | VFloat fa, VInt nb -> VFloat (f_float fa (float_of_int nb))

let as_float = function
  | VInt n -> float_of_int n
  | VFloat f -> f

external log10_c : float -> float = "calc_log10"

let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
  | Op (Add, a, b) -> eval_number_op ( + ) ( +. ) (eval a) (eval b)
  | Op (Mul, a, b) -> eval_number_op ( * ) ( *. ) (eval a) (eval b)
  | Op (Div, a, b) -> eval_number_op ( / ) ( /. ) (eval a) (eval b)
  | Call ("sin", e) -> VFloat (Stdlib.sin (as_float (eval e)))
  | Call ("log10", e) -> VFloat (log10_c (as_float (eval e)))
  | Call _ -> failwith "unknown function"

let info = Cmdliner.Cmd.info "calc"

let eval_lb ~debug_ast lb =
  try
    let expr = Parser.main Lexer.token lb in
    if debug_ast then Format.eprintf "[debug] %a\n" Ast.pp_exp expr;
    let v = eval expr in
    Printf.printf "%s\n" (value_to_string v)
  with Parser.Error ->
    Printf.printf "parse error near character %d" lb.lex_curr_pos

let repl ~debug_ast =
  while true do
    Printf.printf ">> %!";
    let lb = Lexing.from_channel Stdlib.stdin in
    eval_lb ~debug_ast lb
  done

let term =
  let open Cmdliner.Term.Syntax in
  let+ expr_opt =
    let open Cmdliner.Arg in
    value & opt (some string) None & info [ "e" ]
  and+ debug_ast =
    let open Cmdliner.Arg in
    value & flag & info [ "debug-ast" ]
  in
  match expr_opt with
  | Some s -> eval_lb ~debug_ast (Lexing.from_string s)
  | None -> repl ~debug_ast

let cmd = Cmdliner.Cmd.v info term
let main () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
