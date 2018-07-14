{
open StdLabels
open Printf

let fname = Sys.argv.(1)
let fname_gen = fname ^ ".generated"

let lexeme_len lb =
  Lexing.lexeme_end lb - Lexing.lexeme_start lb

let fail lb msg =
  let start = Lexing.lexeme_start_p lb in
  let stop = Lexing.lexeme_end_p lb in
  Printf.eprintf
    "File %S, line %d, characters %d-%d:\n\
     Error: %s\n%!"
    fname start.pos_lnum (start.pos_cnum - start.pos_bol)
    (stop.pos_cnum - start.pos_bol)
    msg;
  exit 1

(* Line number in generated file *)
let lnum = ref 1

let ps s =
  String.iter s ~f:(function
    | '\n' -> incr lnum
    | _    -> ());
  print_string s
let pc = function
  | '\n' -> incr lnum; print_char '\n'
  | c    -> print_char c
let npc n c = for _ = 1 to n do pc c done
let pf fmt = ksprintf ps fmt

let gen_id =
  let n = ref 0 in
  fun () ->
    incr n;
    sprintf "__x%d__" !n

let buf = Buffer.create 512
let add_lexeme lb = Buffer.add_string buf (Lexing.lexeme lb)

type mode = Generated_code | Source_code

let mode = ref Generated_code

let enter_generated_code () =
  mode := Generated_code;
  pc '\n';
  pf "# %d %S\n" !lnum fname_gen

let enter_source_code (pos : Lexing.position) =
  mode := Source_code;
  pc '\n';
  pf "# %d %S\n" pos.pos_lnum fname;
  let col = pos.pos_cnum - pos.pos_bol in
  npc col ' '

let pass_through lb =
  if !mode = Generated_code then
    enter_source_code (Lexing.lexeme_start_p lb);
  ps (Lexing.lexeme lb)

type and_or_in = And | In
}

let space = [' ' '\t']
let newline = '\n' | "\r\n"
let id =  ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']*

rule main col = parse
  | eof
    { In }
  | newline
    { pass_through lexbuf;
      Lexing.new_line lexbuf;
      after_newline col lexbuf
    }
  | " in" newline
    { pass_through lexbuf;
      Lexing.new_line lexbuf;
      after_in_and_newline col lexbuf
    }
  | _
    { pass_through lexbuf;
      main col lexbuf
    }
  | id as id
    { pass_through lexbuf;
      match id with
      | "let" -> after_let col (Lexing.lexeme_start_p lexbuf) lexbuf
      | _     -> main col lexbuf
    }

and after_let col pos = parse
  | "%" (id as id)
    { if id <> "map" then begin
        pass_through lexbuf;
        main col lexbuf
      end else begin
        let col' = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        let rec loop () =
          let pos = Lexing.lexeme_end_p lexbuf in
          enter_generated_code ();
          let id = gen_id () in
          ps id;
          lhs lexbuf;
          let pattern = Buffer.contents buf in
          Buffer.clear buf;
          (id, (pos, pattern)) ::
          match main col' lexbuf with
          | And -> loop ()
          | In  -> []
        in
        let ids, patterns = List.split (loop ()) in
        enter_generated_code ();
        ps "Let_syntax.(fun f -> const f";
        List.iter ids ~f:(pf "$ %s");
        ps ") @@ fun ";
        List.iter patterns ~f:(fun (pos, pattern) ->
          pc '(';
          enter_source_code pos;
          ps pattern;
          pc ')');
        ps "->";
        mode := Generated_code;
        main col lexbuf
      end
    }
  | ""
    { main col lexbuf }

and after_newline col = parse
  | space*
    { pass_through lexbuf;
      if lexeme_len lexbuf = col then
        after_indent col lexbuf
      else
        main col lexbuf
    }

and after_indent col = parse
  | id as id
    { pass_through lexbuf;
      match id with
      | "and" -> And
      | "in"  -> In
      | _     -> fail lexbuf "'and' or 'in' keyword expected"
    }

and after_in_and_newline col = parse
  | space* newline
    { pass_through lexbuf;
      Lexing.new_line lexbuf;
      after_in_and_newline col lexbuf
    }
  | space*
    { pass_through lexbuf;
      if lexeme_len lexbuf = col then
        In
      else
        main col lexbuf
    }

and lhs = parse
  | eof
    { ()
    }
  | newline as s
    { Buffer.add_string buf s;
      Lexing.new_line lexbuf;
      lhs lexbuf
    }
  | "="
    { pass_through lexbuf
    }
  | _ as c
    { Buffer.add_char buf c;
      lhs lexbuf
    }

{
  let (And | In) = main (-1) (Lexing.from_channel (open_in_bin fname))
}
