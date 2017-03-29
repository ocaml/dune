{
open StdLabels
open Printf
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
module Print_diff = struct
  let patdiff_cmd ~use_color =
    let args =
      List.concat [
        ["-keep-whitespace"];
        ["-location-style omake"];
        (if use_color then ["-unrefined"] else ["-ascii"]);
      ]
    in
    String.concat ~sep:" " ("patdiff" :: args)

  let print ?diff_command ?(use_color=false) ~file1 ~file2 () =
    let exec cmd =
      let cmd =
        Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1) (Filename.quote file2)
      in
      match Sys.command cmd with
      | 0 -> true
      | 1 -> false
      | n -> Printf.eprintf "%S exited with code %d\n" cmd n; exit 2
    in
    match diff_command with
    | Some s -> ignore (exec s : bool)
    | None ->
      if exec (patdiff_cmd ~use_color) then (
        Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
        ignore (exec "diff -u" : bool);
      )
end

let main () =
  let fn = Sys.argv.(1) in
  let ic = open_in_bin fn in
  let len = in_channel_length ic in
  let txt = really_input_string ic len in
  close_in ic;
  let chunks =
    let lexbuf = Lexing.from_string txt in
    lexbuf.lex_curr_p <-
      { pos_fname = fn
      ; pos_cnum  = 0
      ; pos_lnum  = 1
      ; pos_bol   = 0
      };
    code txt lexbuf.lex_curr_p lexbuf
  in

  Toploop.initialize_toplevel_env ();
  List.iter
    [ "src"
    ; "vendor/re"
    ]
    ~f:(Topdirs.dir_directory);

  let buf = Buffer.create (len + 1024) in
  let ppf = Format.formatter_of_buffer buf in
  List.iter chunks ~f:(fun (pos, s) ->
    Format.fprintf ppf "%s[%%%%expect{|@." s;
    let lexbuf = Lexing.from_string s in
    lexbuf.lex_curr_p <- pos;
    let phrases = !Toploop.parse_use_file lexbuf in
    List.iter phrases ~f:(fun phr ->
      ignore (Toploop.execute_phrase true ppf phr : bool));
    Format.fprintf ppf "@?|}]@.");
  let res = Buffer.contents buf in

  let corrected_fn = fn ^ ".corrected" in
  (* Temporary hack: *)
  Sys.chdir "../..";
  if txt <> res then begin
    let oc = open_out_bin corrected_fn in
    output_string oc res;
    close_out oc;
    Print_diff.print () ~file1:fn ~file2:corrected_fn;
    exit 1
  end else begin
    if Sys.file_exists corrected_fn then Sys.remove corrected_fn;
    exit 0
  end

let () =
  try
    main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
}
