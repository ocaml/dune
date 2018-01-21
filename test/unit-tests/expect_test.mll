{
open StdLabels
}

rule code txt start = parse
  | "[%%expect{|\n" {
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    (start, s) :: expectation txt lexbuf
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
        [(start, s)]
    end else
      []
  }

and expectation txt = parse
  | "|}]\n" {
      Lexing.new_line lexbuf;
      code txt lexbuf.lex_curr_p lexbuf
    }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    expectation txt lexbuf
  }

{
let main () =
  Test_common.run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
    let chunks = code file_contents lexbuf.lex_curr_p lexbuf in

    Toploop.initialize_toplevel_env ();
    List.iter
      [ "src/.jbuilder.objs"
      ]
      ~f:Topdirs.dir_directory;

    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    List.iter chunks ~f:(fun (pos, s) ->
      Format.fprintf ppf "%s[%%%%expect{|@." s;
      let lexbuf = Lexing.from_string s in
      lexbuf.lex_curr_p <- pos;
      let phrases = !Toploop.parse_use_file lexbuf in
      List.iter phrases ~f:(fun phr ->
        try
          ignore (Toploop.execute_phrase true ppf phr : bool)
        with exn ->
          Location.report_exception ppf exn
      );
      Format.fprintf ppf "@?|}]@.");
    Buffer.contents buf)

let () =
  try
    main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
}
