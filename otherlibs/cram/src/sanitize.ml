open Stdune
open Cmdliner
open Import.Let_syntax

let sanitizer = Fdecl.create Dyn.Encoder.opaque

let set s = Fdecl.set sanitizer s

let term =
  let+ exit_code =
    Arg.(
      value & opt int 0
      & info [ "exit-code" ] ~docv:"NUM"
          ~doc:"Exit code of the shell command we are sanitizing the output of.")
  and+ file =
    Arg.(value & pos 0 (some string) None (Arg.info [] ~docv:"FILE"))
  in
  let sanitize s =
    let sanitizier = Fdecl.get sanitizer in
    Ansi_color.strip s |> Sanitizer.apply sanitizier
  in
  ( match file with
  | Some fn ->
    Io.String_path.read_file fn
    |> sanitize |> String.split_lines
    |> List.iter ~f:(fun line -> Printf.printf "  %s\n" line)
  | None -> (
    let isatty = Unix.(isatty stdout) in
    try
      while true do
        let line = input_line stdin in
        Printf.printf "  %s\n" (sanitize line);
        if isatty then flush stdout
      done
    with End_of_file -> () ) );
  if exit_code <> 0 then Printf.printf "  [%d]\n" exit_code

let command =
  (term, Term.info "sanitize" ~doc:"Sanitize the output of shell phrases.")
