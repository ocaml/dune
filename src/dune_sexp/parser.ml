open Stdune

let error (loc : Loc.t) message = User_error.raise ~loc [ Pp.text message ]

(* To avoid writing two parsers, one for the Cst and one for the Ast, we write
   only one that work for both.

   The natural thing to do would be to have parser that produce [Cst.t] value
   and drop comment for the [Ast.t] one. However the most used parser is the one
   producing Ast one, so it is the one we want to go fast. As a result, we
   encode comment as special [Ast.t] values and decode them for the [Cst.t]
   parser.

   We could also do clever things with GADTs, but it will add type variables
   everywhere which is annoying. *)
module Encoded : sig
  type t

  val template : Template.t -> t
  val atom : Loc.t -> Atom.t -> t
  val quoted_string : Loc.t -> string -> t
  val comment : Loc.t -> string list -> t
  val list : Loc.t -> t list -> t
  val to_csts : t list -> Cst.t list
  val to_asts : t list -> Ast.t list
end = struct
  open Ast

  type t = Ast.t

  let comment_marker : Template.t =
    (* In this value, both the location and template values are non-sensical:

       - the location ends before it starts

       - the template is empty and un-quoted *)
    let loc : Loc.t =
      Loc.create
        ~start:{ pos_fname = ""; pos_cnum = -1; pos_lnum = -1; pos_bol = -1 }
        ~stop:{ pos_fname = ""; pos_cnum = -2; pos_lnum = -2; pos_bol = -2 }
    in
    { loc; parts = []; quoted = false }
  ;;

  let template t =
    assert (t <> comment_marker);
    Template t
  ;;

  let atom loc a = Atom (loc, a)
  let quoted_string loc s = Quoted_string (loc, s)

  let comment loc lines =
    List
      ( loc
      , Template comment_marker
        :: List.map lines ~f:(fun line -> Quoted_string (Loc.none, line)) )
  ;;

  let list loc l = List (loc, l)
  let to_asts l = l

  let rec to_cst (x : Ast.t) : Cst.t =
    match x with
    | Template t -> Template t
    | Quoted_string (loc, s) -> Quoted_string (loc, s)
    | Atom (loc, a) -> Atom (loc, a)
    | List (loc, Template x :: l) when x = comment_marker ->
      Comment
        ( loc
        , List.map l ~f:(function
            | Quoted_string (_, s) -> s
            | _ -> assert false) )
    | List (loc, l) -> List (loc, to_csts l)

  and to_csts l = List.map l ~f:to_cst
end

module Mode = struct
  type 'a t =
    | Single : Ast.t t
    | Many : Ast.t list t
    | Many_as_one : Ast.t t
    | Cst : Cst.t list t

  let with_comments : type a. a t -> bool = function
    | Single -> false
    | Many -> false
    | Many_as_one -> false
    | Cst -> true
  ;;

  let make_result : type a. a t -> Lexing.lexbuf -> Encoded.t list -> a =
    fun t lexbuf sexps ->
    match t with
    | Single ->
      (match Encoded.to_asts sexps with
       | [ sexp ] -> sexp
       | [] -> error (Loc.of_lexbuf lexbuf) "no s-expression found in input"
       | _ :: sexp :: _ -> error (Ast.loc sexp) "too many s-expressions found in input")
    | Many -> Encoded.to_asts sexps
    | Many_as_one ->
      (match Encoded.to_asts sexps with
       | [] -> List (Loc.in_file (Path.of_string lexbuf.lex_curr_p.pos_fname), [])
       | x :: l ->
         let last = List.fold_left l ~init:x ~f:(fun _ x -> x) in
         let loc = Loc.set_stop (Ast.loc x) (Ast.loc last |> Loc.stop) in
         List (loc, x :: l))
    | Cst -> Encoded.to_csts sexps
  ;;
end

let rec loop with_comments depth lexer lexbuf acc =
  match (lexer ~with_comments lexbuf : Lexer.Token.t) with
  | Atom a ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Encoded.atom loc a :: acc)
  | Quoted_string s ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Encoded.quoted_string loc s :: acc)
  | Template t ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Encoded.template { t with loc } :: acc)
  | Lparen ->
    let start = Lexing.lexeme_start_p lexbuf in
    let sexps = loop with_comments (depth + 1) lexer lexbuf [] in
    let stop = Lexing.lexeme_end_p lexbuf in
    let loc = Loc.create ~start ~stop in
    loop with_comments depth lexer lexbuf (Encoded.list loc sexps :: acc)
  | Rparen ->
    if depth = 0
    then
      error (Loc.of_lexbuf lexbuf) "right parenthesis without matching left parenthesis";
    List.rev acc
  | Eof ->
    if depth > 0 then error (Loc.of_lexbuf lexbuf) "unclosed parenthesis at end of input";
    List.rev acc
  | Comment lines ->
    if not with_comments
    then loop false depth lexer lexbuf acc
    else (
      let loc = Loc.of_lexbuf lexbuf in
      loop with_comments depth lexer lexbuf (Encoded.comment loc lines :: acc))
;;

let parse ~mode ?(lexer = Lexer.token) lexbuf =
  let with_comments = Mode.with_comments mode in
  loop with_comments 0 lexer lexbuf [] |> Mode.make_result mode lexbuf
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
