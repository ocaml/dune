{ open Import }

rule iter src repl oc = parse
  | ("# " ['0'-'9']+ " \"" as before) ([^'"' '\n']* as name) ('"' '\r'? '\n' as after)
    { output_string oc before;
      output_string oc (if name = src then repl else name);
      output_string oc after;
      iter src repl oc lexbuf }
  | [^'\n']* '\n' as s
    { output_string oc s;
      iter src repl oc lexbuf }
  | [^'\n']* eof as s
    { output_string oc s }

{
  let rewrite ~src ~dst ~repl =
    with_file_in src ~f:(fun ic ->
      with_file_out dst ~f:(fun oc ->
        iter src repl oc (Lexing.from_channel ic)))
}
