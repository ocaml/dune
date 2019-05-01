{
open StdLabels

type kind = Ignore | Expect | Error
}

let newline = eof | '\r'? '\n'

rule code txt start = parse
  | "[%%ignore]\n" {
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    (Ignore, start, s) :: code txt lexbuf.lex_curr_p lexbuf
  }
  | "[%%expect{|\n" {
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    (Expect, start, s) :: expectation txt lexbuf
  }
  | "[%%error{|\n" {
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    (Error, start, s) :: expectation txt lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    code txt start lexbuf
  }
  | eof {
    let pos = start.Lexing.pos_cnum in
    let len = String.length txt - pos in
    if pos > 0 then begin
      let s = String.sub txt ~pos ~len in
      if String.trim s = "" then
        []
      else
        [(Expect, start, s)]
    end else
      []
  }

and expectation txt = parse
  | "|}]" newline {
      Lexing.new_line lexbuf;
      code txt lexbuf.lex_curr_p lexbuf
    }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    expectation txt lexbuf
  }

{
module Outcometree_cleaner = struct
  open Outcometree

  let lid s =
    match String.rindex s '.' with
    | exception Not_found -> s
    | i ->
      let pos = i + 1 in
      let len = String.length s in
      String.sub s ~pos ~len:(len - pos)

  let ident = function
    | Oide_dot (_, s) -> Oide_ident (lid s)
    | Oide_ident s -> Oide_ident (lid s)
    | id -> id

  let rec value = function
    | Oval_array l -> Oval_array (values l)
    | Oval_constr (id, l) -> Oval_constr (ident id, values l)
    | Oval_list l -> Oval_list (values l)
    | Oval_record l ->
      Oval_record (List.map l ~f:(fun (id, v) -> ident id, value v))
    | Oval_tuple l -> Oval_tuple (values l)
    | Oval_variant (s, Some v) -> Oval_variant (s, Some (value v))
    | v -> v

  and values l = List.map l ~f:value

  let () =
    let print_out_value = !Toploop.print_out_value in
    Toploop.print_out_value := (fun ppf v -> print_out_value ppf (value v))
end

let capture_outputs ~f =
  let temp_file = Filename.temp_file "dune-test" ".output" in
  at_exit (fun () -> Sys.remove temp_file);
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let real_err_formatter =
    Format.formatter_of_out_channel (Unix.out_channel_of_descr stderr_backup)
  in
  let restore () =
    Unix.dup2 stdout_backup Unix.stdout;
    Unix.dup2 stderr_backup Unix.stderr;
    Unix.close stdout_backup;
    Format.pp_print_flush real_err_formatter ();
    Unix.close stderr_backup
  in
  Stdune.Exn.protect
    ~f:(fun () -> f real_err_formatter)
    ~finally:restore;
  Stdune.Io.String_path.read_file temp_file

let flush_all () =
  flush stdout;
  flush stderr

let run real_err_formatter file_contents (lexbuf : Lexing.lexbuf) =
  let chunks = code file_contents lexbuf.lex_curr_p lexbuf in

  Toploop.initialize_toplevel_env ();
  List.iter
    [ "src/dune_lang/.dune_lang.objs/byte"
    ; "src/stdune/.stdune.objs/byte"
    ; "src/fiber/.fiber.objs/byte"
    ; "src/dag/.dag.objs/byte"
    ; "src/memo/.memo.objs/byte"
    ; "src/.dune.objs/byte"
    ]
    ~f:Topdirs.dir_directory;

  List.iter chunks ~f:(fun (kind, pos, s) ->
    begin match kind with
    | Ignore -> Format.printf "%s[%%%%ignore]@." s
    | Expect -> Format.printf "%s[%%%%expect{|@." s
    | Error -> Format.printf "%s[%%%%error{|@." s
    end;
    let lexbuf = Lexing.from_string s in
    lexbuf.lex_curr_p <- pos;
    let phrases = !Toploop.parse_use_file lexbuf in
    List.iter phrases ~f:(fun phr ->
      begin try
        let print_types_and_values =
          match kind with
          | Expect | Error -> true
          | Ignore -> false
        in
        ignore (Toploop.execute_phrase print_types_and_values
                  Format.std_formatter phr : bool)
      with exn ->
        let ppf =
          match kind with
          | Error -> Format.std_formatter
          | Ignore | Expect -> real_err_formatter
        in
        Location.report_exception ppf exn
      end;
      flush_all ()
    );
    begin match kind with
    | Ignore -> ()
    | Expect | Error -> Format.printf "@?|}]@."
    end)

let () =
  try
    Clflags.real_paths := false;
    Test_common.run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
      capture_outputs ~f:(fun ppf -> run ppf file_contents lexbuf))
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
}
