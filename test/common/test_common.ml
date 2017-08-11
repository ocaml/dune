open StdLabels

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

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let run_expect_test file ~f =
  let file_contents = read_file file in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file
    ; pos_cnum  = 0
    ; pos_lnum  = 1
    ; pos_bol   = 0
    };

  let expected = f file_contents lexbuf in

  (* Temporary hack, if we are in the default context, put the .corrected in the source
     tree: *)
  let concat a b =
    match b with
    | ".." -> Filename.dirname a
    | "."  -> a
    | _    -> Filename.concat a b
  in
  let rec loop path after =
    let basename = Filename.basename path in
    if basename = "_build" then
      match after with
      | "default" :: after ->
        List.fold_left after ~init:(Filename.dirname path) ~f:concat
      | _ ->
        List.fold_left after ~init:path ~f:concat
    else
      loop (Filename.dirname path) (basename :: after)
  in
  let file = loop (Filename.concat (Sys.getcwd ()) file) [] in
  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then begin
    let oc = open_out_bin corrected_file in
    output_string oc expected;
    close_out oc;
    Print_diff.print () ~file1:file ~file2:corrected_file;
    exit 1
  end else begin
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0
  end
