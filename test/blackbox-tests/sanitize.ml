open Printf

let cwd = Sys.getcwd ()

let process_line =
  let cwd_re = Str.regexp_string cwd in
  fun line ->
    line
    |> Str.global_replace cwd_re {|$TESTCASE_ROOT|}

let () =
  let files = ref [] in
  let anon s = files := s :: !files in
  let usage = sprintf "%s [FILES]" (Filename.basename Sys.executable_name) in
  Arg.parse [] anon usage;
  List.iter (fun f ->
    if Sys.file_exists f then (
      let ic = open_in f in
      let rec loop lines () =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> loop (process_line line::lines) () in
      let lines = List.rev (loop [] ()) in
      close_in ic;
      let oc = open_out f in
      List.iter (fun line ->
        Printf.fprintf oc "%s\n" line
      ) lines;
      close_out oc
    )
  ) !files
