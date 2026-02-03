open Stdune

let error (loc : Loc.t) message = User_error.raise ~loc [ Pp.text message ]

(* To avoid writing two parsers, one for the Cst and one for the Ast, we write
   only one that works for both.

   The natural thing to do would be to have a parser that produces [Cst.t]
   values and drops comments for the [Ast.t] one. However the most used parser
   is the one producing Ast values, so it is the one we want to go fast. The
   output type is therefore selected by a GADT, allowing the parser to construct
   both representations directly. *)
module Output = struct
  type _ t =
    | Ast : Ast.t t
    | Cst : Cst.t t

  let with_comments : type a. a t -> bool = function
    | Ast -> false
    | Cst -> true
  ;;
end

module Mode = struct
  type 'a t =
    | Single : Ast.t t
    | Many : Ast.t list t
    | Many_as_one : Ast.t t
    | Cst : Cst.t list t
end

let rec loop : type a. a Output.t -> int -> Lexer.t -> Lexing.lexbuf -> a list -> a list =
  fun output depth lexer lexbuf acc ->
  match (lexer ~with_comments:(Output.with_comments output) lexbuf : Lexer.Token.t) with
  | Atom atom ->
    let loc = Loc.of_lexbuf lexbuf in
    let sexp : a =
      match output with
      | Ast -> Ast.Atom (loc, atom)
      | Cst -> Cst.Atom (loc, atom)
    in
    loop output depth lexer lexbuf (sexp :: acc)
  | Quoted_string string ->
    let loc = Loc.of_lexbuf lexbuf in
    let sexp : a =
      match output with
      | Ast -> Ast.Quoted_string (loc, string)
      | Cst -> Cst.Quoted_string (loc, string)
    in
    loop output depth lexer lexbuf (sexp :: acc)
  | Block_string block_string ->
    let loc = Loc.of_lexbuf lexbuf in
    let sexp : a =
      match output with
      | Ast -> Block_string.to_ast ~loc block_string
      | Cst -> Cst.Block_string (loc, block_string)
    in
    loop output depth lexer lexbuf (sexp :: acc)
  | Template template ->
    let loc = Loc.of_lexbuf lexbuf in
    let sexp : a =
      match output with
      | Ast -> Ast.Template { template with loc }
      | Cst -> Cst.Template { template with loc }
    in
    loop output depth lexer lexbuf (sexp :: acc)
  | Lparen ->
    let start = Lexing.lexeme_start_p lexbuf in
    let sexps = loop output (depth + 1) lexer lexbuf [] in
    let stop = Lexing.lexeme_end_p lexbuf in
    let loc = Loc.create ~start ~stop in
    let sexp : a =
      match output with
      | Ast -> Ast.List (loc, sexps)
      | Cst -> Cst.List (loc, sexps)
    in
    loop output depth lexer lexbuf (sexp :: acc)
  | Rparen ->
    if depth = 0
    then
      error (Loc.of_lexbuf lexbuf) "right parenthesis without matching left parenthesis";
    List.rev acc
  | Eof ->
    if depth > 0 then error (Loc.of_lexbuf lexbuf) "unclosed parenthesis at end of input";
    List.rev acc
  | Comment lines ->
    (match output with
     | Ast -> loop output depth lexer lexbuf acc
     | Cst ->
       let loc = Loc.of_lexbuf lexbuf in
       loop output depth lexer lexbuf (Cst.Comment (loc, lines) :: acc))
;;

let parse : type result. mode:result Mode.t -> ?lexer:Lexer.t -> Lexing.lexbuf -> result =
  fun ~mode ?(lexer = Lexer.token) lexbuf ->
  match mode with
  | Cst -> loop Output.Cst 0 lexer lexbuf []
  | Single ->
    (match loop Output.Ast 0 lexer lexbuf [] with
     | [ sexp ] -> sexp
     | [] -> error (Loc.of_lexbuf lexbuf) "no s-expression found in input"
     | _ :: sexp :: _ -> error (Ast.loc sexp) "too many s-expressions found in input")
  | Many -> loop Output.Ast 0 lexer lexbuf []
  | Many_as_one ->
    (match loop Output.Ast 0 lexer lexbuf [] with
     | [] -> Ast.List (Loc.in_file (Path.of_string lexbuf.lex_curr_p.pos_fname), [])
     | sexp :: sexps ->
       let last = List.fold_left sexps ~init:sexp ~f:(fun _ sexp -> sexp) in
       let loc = Loc.set_stop (Ast.loc sexp) (Loc.stop (Ast.loc last)) in
       Ast.List (loc, sexp :: sexps))
;;

let parse_string ~fname ~mode ?lexer str =
  let lb = Lexbuf.from_string ~fname str in
  parse ~mode ?lexer lb
;;

let load ?lexer path ~mode = Io.with_lexbuf_from_file path ~f:(parse ~mode ?lexer)

let insert_comments csts comments =
  (* To insert the comments, we tokenize the csts, reconciliate the token
     streams and parse the result again. This is not the fastest implementation,
     but at least it is simple. *)
  let compare (a, _) (b, _) = Int.compare (Loc.start a).pos_cnum (Loc.start b).pos_cnum in
  let rec reconciliate acc tokens1 tokens2 =
    match tokens1, tokens2 with
    | [], l | l, [] -> List.rev_append acc l
    | tok1 :: rest1, tok2 :: rest2 ->
      (match compare tok1 tok2 with
       | Eq | Lt -> reconciliate (tok1 :: acc) rest1 tokens2
       | Gt -> reconciliate (tok2 :: acc) tokens1 rest2)
  in
  let tokens =
    reconciliate
      []
      (Cst.tokenize csts)
      (List.sort comments ~compare
       |> List.map ~f:(fun (loc, comment) -> loc, Lexer.Token.Comment comment))
  in
  let tokens = ref tokens in
  let lexer ~with_comments:_ (lb : Lexing.lexbuf) =
    match !tokens with
    | [] ->
      lb.lex_curr_p <- lb.lex_start_p;
      Lexer.Token.Eof
    | (loc, tok) :: rest ->
      tokens := rest;
      lb.lex_start_p <- Loc.start loc;
      lb.lex_curr_p <- Loc.stop loc;
      tok
  in
  parse (Lexing.from_string "") ~lexer ~mode:Cst
;;
