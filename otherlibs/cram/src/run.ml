open Stdune
open Cmdliner
open Import.Let_syntax

(* Translate a path for [sh]. On Windows, [sh] will come from Cygwin so if we
   are a real windows program we need to pass the path through [cygpath] *)
let translate_path_for_sh =
  if not Sys.win32 then
    fun fn ->
  fn
  else
    fun fn ->
  let fdr, fdw = Unix.pipe () in
  Unix.set_close_on_exec fdr;
  let pid =
    Unix.create_process "cygpath" [| "cygpath"; fn |] Unix.stdin fdw Unix.stderr
  in
  Unix.close fdw;
  let ic = Unix.in_channel_of_descr fdr in
  let fn = input_line ic in
  close_in ic;
  assert (snd (Unix.waitpid [] pid) = Unix.WEXITED 0);
  fn

(* Quote a filename for sh, independently of whether we are on Windows or Unix.
   On Windows, we still generate a [sh] script so we need to quote using Unix
   conventions. *)
let quote_for_sh fn =
  let buf = Buffer.create (String.length fn + 2) in
  Buffer.add_char buf '\'';
  String.iter fn ~f:(function
    | '\'' -> Buffer.add_string buf "'\\''"
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '\'';
  Buffer.contents buf

let run_expect_test file ~f =
  let file_contents = Io.String_path.read_file file in
  let lexbuf = Lexbuf.from_string file_contents ~fname:file in
  let expected = f lexbuf in
  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then
    Io.String_path.write_file corrected_file expected
  else (
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0
  )

let temp_file suffix =
  let fn = Filename.temp_file "dune-test" suffix in
  at_exit (fun () -> try Sys.remove fn with _ -> ());
  fn

let open_temp_file suffix =
  let fn, oc =
    Filename.open_temp_file "dune-test" suffix ~mode:[ Open_binary ]
  in
  at_exit (fun () -> try Sys.remove fn with _ -> ());
  (fn, oc)

let extend_build_path_prefix_map ~cwd =
  let var = "BUILD_PATH_PREFIX_MAP" in
  let s =
    Build_path_prefix_map.encode_map
      [ Some { source = cwd; target = "$TESTCASE_ROOT" } ]
  in
  let s =
    match Sys.getenv var with
    | exception Not_found -> s
    | s' -> s ^ ":" ^ s'
  in
  Unix.putenv var s

let run ~sanitizer ~file lexbuf =
  let sanitizer_command =
    match sanitizer with
    | None ->
      quote_for_sh (translate_path_for_sh Sys.executable_name) ^ " sanitize"
    | Some prog ->
      let prog =
        if Filename.is_relative prog then
          Filename.concat (Sys.getcwd ()) prog
        else
          prog
      in
      translate_path_for_sh prog |> quote_for_sh
  in
  Sys.chdir (Filename.dirname file);
  let cwd = Sys.getcwd () in
  Unix.putenv "LC_ALL" "C";
  extend_build_path_prefix_map ~cwd;
  let script, oc = open_temp_file ".sh" in
  let prln fmt = Printf.fprintf oc (fmt ^^ "\n") in
  (* Shell code written by the user might not be properly terminated. For
     instance the user might forgot to write [EOF] after a [cat <<EOF]. If we
     wrote this shell code directly in the main script, it would hide the rest
     of the script. So instead, we dump each user written shell phrase into a
     file and then source it in the main script. *)
  let user_shell_code_file = temp_file ".sh" in
  let user_shell_code_file_sh_path =
    translate_path_for_sh user_shell_code_file
  in
  (* Where we store the output of shell code written by the user *)
  let user_shell_code_output_file = temp_file ".output" in
  let user_shell_code_output_file_sh_path =
    translate_path_for_sh user_shell_code_output_file
  in

  (* Produce the following shell code:

     {v cat <<"EOF_<n>" <data> EOF_<n> v}

     choosing [<n>] such that [CRAM_EOF_<n>] doesn't appear in [<data>]. *)
  let cat_eof ?dest lines =
    let rec loop n =
      let sentinel =
        if n = 0 then
          "EOF"
        else
          sprintf "EOF%d" n
      in
      if List.mem sentinel ~set:lines then
        loop (n + 1)
      else
        sentinel
    in
    let sentinel = loop 0 in
    ( match dest with
    | None -> prln "cat <<%S" sentinel
    | Some fn -> prln "cat >%s <<%S" (quote_for_sh fn) sentinel );
    List.iter lines ~f:(prln "%s");
    prln "%s" sentinel
  in

  let rec loop () =
    match Cram_lexer.block lexbuf with
    | None -> close_out oc
    | Some block -> (
      match block with
      | Comment lines ->
        cat_eof lines;
        loop ()
      | Command lines ->
        cat_eof
          (List.mapi lines ~f:(fun i line ->
               if i = 0 then
                 "  $ " ^ line
               else
                 "  > " ^ line));
        cat_eof lines ~dest:user_shell_code_file;
        prln ". %s > %s 2>&1"
          (quote_for_sh user_shell_code_file_sh_path)
          (quote_for_sh user_shell_code_output_file_sh_path);
        prln {|%s --exit-code $? < %s|} sanitizer_command
          (quote_for_sh user_shell_code_output_file_sh_path);
        loop () )
  in
  loop ();
  let output_file = temp_file ".output" in
  let fd = Unix.openfile output_file [ O_WRONLY; O_TRUNC ] 0 in
  let pid = Unix.create_process "sh" [| "sh"; script |] Unix.stdin fd fd in
  Unix.close fd;
  let n =
    match snd (Unix.waitpid [] pid) with
    | WEXITED n -> n
    | _ -> 255
  in
  let output = Io.String_path.read_file output_file in
  if n <> 0 then (
    Printf.eprintf "Generated cram script exited with code %d!\n" n;
    Printf.eprintf "Script:\n";
    let script = Io.String_path.read_file script in
    List.iter (String.split_lines script) ~f:(Printf.eprintf "| %s\n");
    Printf.eprintf "Script output:\n";
    List.iter (String.split_lines output) ~f:(Printf.eprintf "| %s\n");
    exit 1
  );
  output

let term =
  let+ sanitizer =
    Arg.(
      value
      & opt (some string) None
      & info [ "sanitizer" ] ~docv:"PROG"
          ~doc:"Program used to sanitize the output of shell commands.")
  and+ file =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"FILE"))
  in
  run_expect_test file ~f:(fun lexbuf -> run ~sanitizer ~file lexbuf)

let command = (term, Term.info "run" ~doc:"Run a cram script.")
