/**************************************************************************/
/*                                                                        */
/*    Copyright 2012-2017 OCamlPro                                        */
/*    Copyright 2012 INRIA                                                */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

%{

open OpamParserTypes.FullPos

(** Opam config file generic type parser *)

let pos_of_lexing_pos spos epos =
  Lexing.({
    filename = spos.pos_fname;
    start = spos.pos_lnum, spos.pos_cnum - spos.pos_bol;
    stop = epos.pos_lnum, epos.pos_cnum - epos.pos_bol;
  })

let get_pos_full ?(s=1) n =
  pos_of_lexing_pos (Parsing.rhs_start_pos s) (Parsing.rhs_end_pos n)

let get_pos n = get_pos_full ~s:n n

(* This must match up with the package's version; checked by the build system *)
let version = (2, 1)

%}

%token <string> STRING IDENT
%token <bool> BOOL
%token EOF
%token LBRACKET RBRACKET
%token LPAR RPAR
%token LBRACE RBRACE
%token COLON
%token <int> INT
%token <OpamParserTypes.FullPos.relop_kind> RELOP
%token AND
%token OR
%token <OpamParserTypes.FullPos.pfxop_kind> PFXOP
%token <OpamParserTypes.FullPos.env_update_op_kind> ENVOP

%left COLON
%left ATOM
%left OR
%left AND
%nonassoc ENVOP
%nonassoc PFXOP
%left LBRACE RBRACE
%nonassoc RELOP
%nonassoc URELOP

%start main value
%type <string -> OpamParserTypes.FullPos.opamfile> main
%type <OpamParserTypes.FullPos.value> value
%type <OpamParserTypes.FullPos.value list> values
%type <OpamParserTypes.FullPos.opamfile_item> item

%%

main:
| items EOF { fun file_name ->
        { file_contents = $1; file_name } }
;

items:
| item items { $1 :: $2 }
|            { [] }
;

item:
| IDENT COLON value                {
  { pos = get_pos_full 3;
    pelem =
      Variable ({ pos = get_pos 1; pelem =  $1 }, $3);
  }
}
| IDENT LBRACE items RBRACE {
  { pos = get_pos_full 4;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = None;
                section_items =
                  { pos = get_pos_full ~s:2 4; pelem = $3 };
               })
  }
}
| IDENT STRING LBRACE items RBRACE {
  { pos = get_pos_full 5;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = Some { pos = get_pos 2; pelem = $2 };
                section_items =
                  { pos = get_pos_full ~s:3 5; pelem = $4 };
               })
  }
}
;

value:
| atom            %prec ATOM { $1 }
| LPAR values RPAR           {{ pos = get_pos_full 3 ; pelem = Group { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| LBRACKET values RBRACKET   {{ pos = get_pos_full 3 ; pelem = List { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| value LBRACE values RBRACE {{ pos = get_pos_full 4 ;
                                pelem = Option ($1, { pos = get_pos_full ~s:2 4; pelem = $3 }) }}
| value AND value            {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `And },$1,$3) }}
| value OR value             {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `Or },$1,$3) }}
| atom RELOP atom            {{ pos = get_pos_full 3 ; pelem = Relop ({ pos = get_pos 2 ; pelem = $2 },$1,$3) }}
| atom ENVOP atom            {{ pos = get_pos_full 3 ; pelem = Env_binding ($1,{ pos = get_pos 2 ; pelem = $2 },$3) }}
| PFXOP value                {{ pos = get_pos_full 2 ; pelem = Pfxop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
| RELOP atom                 {{ pos = get_pos_full 2 ; pelem = Prefix_relop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
;

values:
|                            { [] }
| value values               { $1 :: $2 }
;

atom:
| IDENT                      {{ pos = get_pos 1 ; pelem = Ident $1 }}
| BOOL                       {{ pos = get_pos 1 ; pelem = Bool $1 }}
| INT                        {{ pos = get_pos 1 ; pelem = Int $1 }}
| STRING                     {{ pos = get_pos 1 ; pelem = String $1 }}
;

%%

let nopatch v =
  let s =
  try
    let i = String.index v '.' in
    let i = String.index_from v (i+1) '.' in
    (String.sub v 0 i)
  with Not_found ->
    let rec f i =
      if i >= String.length v then v
      else match String.get v i with
        | '0'..'9' | '.' -> f (i+1)
        | _ -> String.sub v 0 i
    in
    f 0
  in
    try Scanf.sscanf s "%u.%u%!" (fun maj min -> (maj, min))
    with Scanf.Scan_failure _
       | Failure _
       | End_of_file ->
           try Scanf.sscanf s "%u%!" (fun maj -> (maj, 0))
           with Scanf.Scan_failure _
              | Failure _
              | End_of_file -> (0, 0)

let with_clear_parser f x =
  try
    let r = f x in
    Parsing.clear_parser ();
    r
  with e ->
    Parsing.clear_parser ();
    raise e

(* Update a lexbuf with position information prior to raising an exception *)
let reset_lexbuf_and_abort l file_name (start_line, start_col) (end_line, end_col) exn =
  let open Lexing in
  l.lex_start_p <- {pos_fname = file_name; pos_lnum = start_line; pos_bol = 0; pos_cnum = start_col};
  l.lex_curr_p <- {pos_fname = file_name; pos_lnum = end_line; pos_bol = 0; pos_cnum = end_col};
  exn ()

(* cf. OpamStd.fatal - always allow standard exceptions to propagate. *)
let not_fatal = function
| Sys.Break
| Assert_failure _
| Match_failure _ -> false
| _ -> true

let get_three_tokens lexer lexbuf = 
  let open Lexing in
  try
    let p0 = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let t1 = lexer lexbuf in
    let p1 = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let t2 = lexer lexbuf in
    let p2 = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    let t3 = lexer lexbuf in
    let p3 = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    ((p0, p1, p2, p3), (t1, t2, t3))
  with
  | e when not_fatal e -> raise Parsing.Parse_error

(* Wrap the ocamlyacc parser *)
let main lexer lexbuf file_name =
  (* Extract the exceptions for opam-version not at the top of the file and
     opam-version duplicated. OpamLexer has special cases for these two
     constants. If OpamLexer.token isn't used, raise Parse_error instead. *)
  let exn_not_first () =
    let _ = lexer (Lexing.from_string "version: \"42\"\nopam-version: \"2.1\"") in
    raise Parsing.Parse_error
  and exn_duplicate () =
    let _ = lexer (Lexing.from_string "opam-version: \"2.1\"\nopam-version: \"z\"") in
    raise Parsing.Parse_error
  and restore_pos (start, curr) =
    let open Lexing in
    lexbuf.lex_start_p <- start;
    lexbuf.lex_curr_p <- curr
  in
  (* Raises the exn_not_first or exn_duplicate exceptions if an invalid
     opam-version variable is found in the result. *)
  let scan_opam_version_variable format_2_1_or_greater = function
  | {pelem = Variable({pelem = "opam-version"; _}, {pelem = String ver; _}); pos = {start; stop; _}} ->
      if format_2_1_or_greater then
        (* [opam-version] can only appear once for 2.1+ *)
        reset_lexbuf_and_abort lexbuf file_name start stop exn_duplicate
      else if nopatch ver > (2, 0) then
        (* Only [opam-version: "2.0"] can appear after the first non-comment/whitespace line of the file *)
        reset_lexbuf_and_abort lexbuf file_name start stop exn_not_first
      else
        ()
  | _ -> ()
  in
  (* Now parse the header of the file manually. The smallest valid opam file
     is `ident: atom`, so if we can't read three tokens we have a parse error *)
  let ((((_, p0) as initial_pos), ((_, p1) as pos1), ((_, p2) as pos2), ((_, p3) as pos3)), (t1, t2, t3)) =
    get_three_tokens lexer lexbuf
  in
  (* Parse those three tokens if they are [opam-version: ver] *)
  let (header, format_2_1_or_greater, trap_exceptions) =
    match (t1, t2, t3) with
    | (IDENT "opam-version", COLON, STRING ver) ->
        let header =
          (* Parsing or lexing errors immediate following opam-version may cause
             an exception to be raised before the element has been fully parsed.
             In this case, we generate a single opam-version Variable to return.
           *)
          {pelem = Variable({pelem = "opam-version"; pos = pos_of_lexing_pos p0 p1},
                             {pelem = String ver; pos = pos_of_lexing_pos p2 p3});
           pos = pos_of_lexing_pos p0 p3}
        in
        (header, (nopatch ver >= (2, 1)), (nopatch ver > version))
    | _ ->
        (* Default is [opam-version: "2.0"] *)
        let pos = {filename = ""; start = (0, 0); stop = (0, 0)} in
        ({pelem = Variable ({pelem = ""; pos}, {pelem = Int 42; pos}); pos}, false, false)
  in
  (* The parser will use position information from the lexbuf, so replay the
     positions, even if we're not actually reading anything. *)
  restore_pos initial_pos;
  (* Wrap the lexer to simulate reading those three tokens a second time *)
  let lexer =
    let tokens = ref [t1, pos1; t2, pos2; t3, pos3] in
    fun lexbuf ->
      match tokens with
      | {contents = (t, p)::rest} ->
          tokens := rest;
          restore_pos p;
          t
      | {contents = []} ->
          lexer lexbuf
  in
  let result =
    try with_clear_parser (main lexer lexbuf) file_name
    with e when trap_exceptions && not_fatal e ->
      (* Append a syntactically invalid sentinel section "#" to the version
         header which was manually parsed. That is then sufficient
         information for a client to determine that the file was invalid.
         If OpamBaseParser.version = (2, 1), this would allow
         `opam-version: "2.2"`, containing no lexer or parser changes, still to
         report syntax errors in opam 2.2, by using this sentinel group to
         detect the parsing error. *)
      let sentinel =
        let pos =
          Lexing.(pos_of_lexing_pos lexbuf.lex_start_p lexbuf.lex_curr_p)
        in
        let section =
          {section_kind = {pelem = "#"; pos};
           section_name = None;
           section_items = {pelem = []; pos}}
        in
          {pelem = Section section; pos}
      in
      {file_contents = [header; sentinel]; file_name}
  in
  begin
    match result with
    | {file_contents = _::items; _} ->
        (* Ensure that there are no `opam-version` fields with a value >= "2.1"
           further down the file. *)
        List.iter (scan_opam_version_variable format_2_1_or_greater) items
    | _ -> ()
  end;
  result

let value t l = with_clear_parser (value t) l
