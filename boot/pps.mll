{
  let add_lexeme dst lexbuf =
    Buffer.add_string dst (Lexing.lexeme lexbuf)
  ;;

  let append_char s c =
    let len = String.length s in
    let bytes = Bytes.create (len + 1) in
    Bytes.blit_string s 0 bytes 0 len;
    Bytes.set bytes len c;
    Bytes.unsafe_to_string bytes
  ;;

  let keep_recent recent ~limit =
    let len = String.length recent in
    if len <= limit then recent else String.sub recent (len - limit) limit
  ;;

  let quoted_string_end id = "|" ^ id ^ "}"

  let ocaml_version =
    Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun a b c -> a, b, c)

  let version a b c =
    int_of_string a, int_of_string b, int_of_string c
  ;;

  let eval_relop relop rhs =
    match relop with
    | "<" -> ocaml_version < rhs
    | "<=" -> ocaml_version <= rhs
    | ">" -> ocaml_version > rhs
    | ">=" -> ocaml_version >= rhs
    | "=" -> ocaml_version = rhs
    | "<>" -> ocaml_version <> rhs
    | _ -> failwith "invalid operator in [%%if]"
  ;;

  let update_end_depth depth = function
    | "begin" | "object" | "sig" | "struct" -> depth + 1
    | "end" when depth > 0 -> depth - 1
    | _ -> depth
  ;;
}

let blank = [' ' '\t' '\n' '\r']*
let integer = ['0'-'9']+
let quoted_string_id = ['a'-'z' '_']*
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let octal_digit = ['0'-'7']
let char_simple = [^ '\\' '\'' '\n' '\r']
let char_escape =
  '\\'
  ([^ '\n' '\r' '0'-'9' 'x' 'o']
  | ['0'-'9']['0'-'9']['0'-'9']
  | 'x' hex_digit hex_digit
  | 'o' ['0'-'3'] octal_digit octal_digit)
let char_literal = '\'' (char_simple | char_escape) '\''
let lower_ident = ['a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*

rule pp_lex dst = parse
 | "let%expect_test" { skip_expect_test (pp_lex dst) 0 lexbuf }
 | "let%test_module" { skip_test_module (pp_lex dst) 0 lexbuf }
 | "[%%if" { copy_conditional dst lexbuf; pp_lex dst lexbuf }
 | '"' { add_lexeme dst lexbuf; copy_string dst (pp_lex dst) lexbuf }
 | (char_literal as c) { Buffer.add_string dst c; pp_lex dst lexbuf }
 | '{' (quoted_string_id as id) '|'
   {
     add_lexeme dst lexbuf;
     copy_quoted_string dst (pp_lex dst) (quoted_string_end id) "" lexbuf
   }
 | "(*" { add_lexeme dst lexbuf; copy_comment dst (pp_lex dst) 1 lexbuf }
 | _ as c { Buffer.add_char dst c; pp_lex dst lexbuf }
 | eof { () }

and skip_expect_test k end_depth = parse
 | ";;"
   {
     if end_depth = 0 then k lexbuf else skip_expect_test k end_depth lexbuf
   }
 | '"' { skip_string (skip_expect_test k end_depth) lexbuf }
 | char_literal { skip_expect_test k end_depth lexbuf }
 | '{' (quoted_string_id as id) '|'
   {
     skip_quoted_string
       (skip_expect_test k end_depth)
       (quoted_string_end id)
       ""
       lexbuf
   }
 | "(*" { skip_comment (skip_expect_test k end_depth) 1 lexbuf }
 | (lower_ident as id)
   { skip_expect_test k (update_end_depth end_depth id) lexbuf }
 | _ { skip_expect_test k end_depth lexbuf }
 | eof { failwith "unterminated let%expect_test" }

and skip_test_module k depth = parse
 | '(' { skip_test_module k (depth + 1) lexbuf }
 | ')'
   {
     if depth = 1
     then skip_expect_test k 0 lexbuf
     else skip_test_module k (depth - 1) lexbuf
   }
 | '"' { skip_string (skip_test_module k depth) lexbuf }
 | char_literal { skip_test_module k depth lexbuf }
 | '{' (quoted_string_id as id) '|'
   {
     skip_quoted_string
       (skip_test_module k depth)
       (quoted_string_end id)
       ""
       lexbuf
   }
 | "(*" { skip_comment (skip_test_module k depth) 1 lexbuf }
 | _ { skip_test_module k depth lexbuf }
 | eof { failwith "unterminated let%test_module" }

and copy_conditional dst = parse
 | blank "ocaml_version" blank
   (("<=" | ">=" | "<>" | "<" | ">" | "=") as relop)
   blank '(' blank
   (integer as major)
   blank ',' blank
   (integer as minor)
   blank ',' blank
   (integer as patch)
   blank ')' blank ']'
   {
     let keep_then = eval_relop relop (version major minor patch) in
     let then_branch = Buffer.create 128 in
     let else_branch = Buffer.create 128 in
     copy_branches then_branch else_branch then_branch false lexbuf;
     Buffer.add_buffer dst (if keep_then then then_branch else else_branch)
   }
 | _ { failwith "invalid [%%if] header" }

and copy_branches then_branch else_branch current seen_else = parse
 | "let%expect_test"
   {
     skip_expect_test
       (copy_branches then_branch else_branch current seen_else)
       0
       lexbuf
   }
 | "let%test_module"
   {
     skip_test_module
       (copy_branches then_branch else_branch current seen_else)
       0
       lexbuf
   }
 | "[%%if"
   {
     copy_conditional current lexbuf;
     copy_branches then_branch else_branch current seen_else lexbuf
   }
 | "[%%else]"
   {
     if seen_else
     then failwith "unexpected [%%else]"
     else copy_branches then_branch else_branch else_branch true lexbuf
   }
 | "[%%endif]" { () }
 | '"' {
     add_lexeme current lexbuf;
     copy_string
       current
       (copy_branches then_branch else_branch current seen_else)
       lexbuf
   }
 | (char_literal as c)
   {
     Buffer.add_string current c;
     copy_branches then_branch else_branch current seen_else lexbuf
   }
 | '{' (quoted_string_id as id) '|'
   {
     add_lexeme current lexbuf;
     copy_quoted_string
       current
       (copy_branches then_branch else_branch current seen_else)
       (quoted_string_end id)
       ""
       lexbuf
   }
 | "(*" {
     add_lexeme current lexbuf;
     copy_comment
       current
       (copy_branches then_branch else_branch current seen_else)
       1
       lexbuf
   }
 | _ as c
   {
     Buffer.add_char current c;
     copy_branches then_branch else_branch current seen_else lexbuf
   }
 | eof { failwith "unterminated [%%if]" }

and skip_string k = parse
 | '"' { k lexbuf }
 | '\\' _ { skip_string k lexbuf }
 | _ { skip_string k lexbuf }
 | eof { failwith "unterminated string literal" }

and skip_quoted_string k terminator recent = parse
 | _ as c
   {
     (* We keep a sliding suffix to recognize the closing [|id}]. This still
        does O(String.length terminator) work per body character, so
        [skip_quoted_string] and [copy_quoted_string] are worst-case quadratic
        in inputs with extremely long quoted-string tags. That cost manifests
        while scanning the body of [{|...|}] and [{id|...|id}] literals. *)
     let next = append_char recent c in
     if next = terminator
     then k lexbuf
     else
       skip_quoted_string
         k
         terminator
         (keep_recent next ~limit:(String.length terminator - 1))
         lexbuf
   }
 | eof { failwith "unterminated quoted string literal" }

and skip_comment k depth = parse
 | "(*" { skip_comment k (depth + 1) lexbuf }
 | "*)" {
     if depth = 1
     then k lexbuf
     else skip_comment k (depth - 1) lexbuf
   }
 | _ { skip_comment k depth lexbuf }
 | eof { failwith "unterminated comment" }

and copy_string dst k = parse
 | '"' {
     add_lexeme dst lexbuf;
     k lexbuf
   }
 | '\\' _ {
     add_lexeme dst lexbuf;
     copy_string dst k lexbuf
   }
 | _ {
     add_lexeme dst lexbuf;
     copy_string dst k lexbuf
   }
 | eof { failwith "unterminated string literal" }

and copy_quoted_string dst k terminator recent = parse
 | _ as c
   {
     Buffer.add_char dst c;
     let next = append_char recent c in
     if next = terminator
     then k lexbuf
     else
       copy_quoted_string
         dst
         k
         terminator
         (keep_recent next ~limit:(String.length terminator - 1))
         lexbuf
   }
 | eof { failwith "unterminated quoted string literal" }

and copy_comment dst k depth = parse
 | "(*" {
     add_lexeme dst lexbuf;
     copy_comment dst k (depth + 1) lexbuf
   }
 | "*)" {
     add_lexeme dst lexbuf;
     if depth = 1
     then k lexbuf
     else copy_comment dst k (depth - 1) lexbuf
   }
 | _ {
     add_lexeme dst lexbuf;
     copy_comment dst k depth lexbuf
   }
 | eof { failwith "unterminated comment" }

{
  let pp s =
    let b = Buffer.create (String.length s) in
    let lb = Lexing.from_string s in
    pp_lex b lb;
    Buffer.contents b
  ;;
}
