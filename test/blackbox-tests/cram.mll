(* Mini implementation of cram tests *)

{
open Dune
open Import

type dot_t_block =
  | Command of string list
  | Comment of string list

(* Translate a path for [sh]. On Windows, [sh] will come from Cygwin
   so if we are a real windows program we need to pass the path
   through [cygpath] *)
let translate_path_for_sh =
  if not Sys.win32 then
    fun fn -> fn
  else
    fun fn ->
      let fdr, fdw = Unix.pipe () in
      Unix.set_close_on_exec fdr;
      let pid =
        Unix.create_process "cygpath" [|"cygpath"; fn|]
          Unix.stdin fdw Unix.stderr
      in
      Unix.close fdw;
      let ic = Unix.in_channel_of_descr fdr in
      let fn = input_line ic in
      close_in ic;
      assert (snd (Unix.waitpid [] pid) = Unix.WEXITED 0);
      fn

(* Quote a filename for sh, independently of whether we are on Windows
   or Unix. On Windows, we still generate a [sh] script so we need to
   quote using Unix conventions. *)
let quote_for_sh fn =
  let buf = Buffer.create (String.length fn + 2) in
  Buffer.add_char buf '\'';
  String.iter fn ~f:(function
    | '\'' -> Buffer.add_string buf "'\\''"
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '\'';
  Buffer.contents buf
}

let eol = '\n' | eof

let blank = [' ' '\t' '\r' '\012']

rule dot_t_block = parse
  | eof { None }
  | "  $ " ([^'\n']* as str) eol
    { Some (command_cont [str] lexbuf) }
  | "  " [^'\n']* eol
    { output [] lexbuf }
  | ' '? as str eol
    { comment [str] lexbuf }
  | ' '? [^' ' '\n'] [^'\n']* as str eol
    { comment [str] lexbuf }

and comment acc = parse
  | eof
    { match acc with
    | [] -> None
    | _ -> Some (Comment (List.rev acc))
    }
  | ' '? as str eol
    { comment (str :: acc) lexbuf }
  | ' '? [^' ' '\n'] [^'\n']* as str eol
    { comment (str :: acc) lexbuf }
  | ""
    { Some (Comment (List.rev acc)) }

and output maybe_comment = parse
  | eof
    { match maybe_comment with
    | [] -> None
    | l -> Some (Comment (List.rev l))
    }
  | ' ' eof
    { Some (Comment (List.rev (" " :: maybe_comment))) }
  | "  "? eof
    { None }
  | "  " eol
    { output [] lexbuf }
  | ' '? as s eol
    { output (s :: maybe_comment) lexbuf }
  | "  $" eol
    { output [] lexbuf }
  | "  " '$' [^' ' '\n'] [^'\n']* eol
    { output [] lexbuf }
  | "  " [^'$' '\n'] [^'\n']* eol
    { output [] lexbuf }
  | ""
    { match maybe_comment with
    | [] -> dot_t_block lexbuf
    | l -> comment l lexbuf
    }

and command_cont acc = parse
  | "  > " ([^'\n']* as str) eol
    { command_cont (str :: acc) lexbuf }
  | "  >" eol
    { command_cont ("" :: acc) lexbuf }
  | ""
    { Command (List.rev acc)  }

{

  type version = int * int * int

  let parse_version s =
    Scanf.sscanf s "%d.%d.%d" (fun a b c -> a, b, c)

  type test =
    | Eq
    | Le
    | Ge
    | Lt
    | Gt

  let tests =
    [ "=" , Eq
    ; "<=", Le
    ; ">=", Ge
    ; "<" , Lt
    ; ">" , Gt
    ; ""  , Eq
    ]

  let test = function
    | Eq -> (=)
    | Ge -> (>=)
    | Le -> (<=)
    | Lt -> (<)
    | Gt -> (>)

  let parse_skip_versions s =
    List.map (String.split s ~on:',') ~f:(fun x ->
      Option.value_exn
        (List.find_map tests ~f:(fun (prefix, test) ->
           Option.map (String.drop_prefix x ~prefix)
             ~f:(fun x -> (test, parse_version x)))))

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
      Filename.open_temp_file "dune-test" suffix
        ~mode:[Open_binary]
    in
    at_exit (fun () -> try Sys.remove fn with _ -> ());
    fn, oc

let () =
  let sanitizer = ref "" in
  let args =
    Arg.align
      [ "-sanitizer"
      , Arg.Set_string sanitizer
      , "PROGRAM Program used to sanitize output"
      ]
  in
  let expect_test = ref None in
  let anon s =
    match !expect_test with
    | None -> expect_test := Some s
    | Some _ ->
      raise (Arg.Bad "Too many input files")
  in
  let usage = sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name) in
  Arg.parse args anon usage;
  let expect_test =
    match !expect_test with
    | None -> raise (Arg.Bad "Expect test file must be passed")
    | Some p -> p
  in
  Unix.putenv "LC_ALL" "C";
  let cwd = Sys.getcwd () in
  run_expect_test expect_test ~f:(fun lexbuf ->
    let script, oc = open_temp_file ".sh" in
    let prln fmt = Printf.fprintf oc (fmt ^^ "\n") in
    (* Shell code written by the user might not be properly
       terminated. For instance the user might forgot to write
       [EOF] after a [cat <<EOF]. If we wrote this shell code
       directly in the main script, it would hide the rest of the
       script. So instead, we dump each user written shell phrase
       into a file and then source it in the main script. *)
    let user_shell_code_file = temp_file ".sh" in
    let user_shell_code_file_sh_path =
      translate_path_for_sh  user_shell_code_file
    in
    (* Where we store the output of shell code written by the user *)
    let user_shell_code_output_file = temp_file ".output" in
    let user_shell_code_output_file_sh_path =
      translate_path_for_sh  user_shell_code_output_file
    in
    let sanitizer =
      if Filename.is_relative !sanitizer then
        Filename.concat (Sys.getcwd ()) !sanitizer
      else
        !sanitizer
    in
    let sanitizer_sh_path = translate_path_for_sh sanitizer in
    (* Produce the following shell code:

       {v
             cat <<"EOF_<n>"
             <data>
             EOF_<n>
           v}

       choosing [<n>] such that [CRAM_EOF_<n>] doesn't appear in
       [<data>]. *)
    let cat_eof ?dest lines =
      let rec loop n =
        let sentinel = if n = 0 then "EOF" else sprintf "EOF%d" n in
        if List.mem sentinel ~set:lines then
          loop (n + 1)
        else
          sentinel
      in
      let sentinel = loop 0 in
      (match dest with
       | None -> prln "cat <<%S" sentinel
       | Some fn -> prln "cat >%s <<%S" (quote_for_sh fn) sentinel);
      List.iter lines ~f:(prln "%s");
      prln "%s" sentinel
    in
    let rec loop () =
      match dot_t_block lexbuf with
      | None -> close_out oc
      | Some block ->
        match block with
        | Comment lines ->
          cat_eof lines;
          loop ()
        | Command lines ->
          cat_eof (List.mapi lines ~f:(fun i line ->
            if i = 0 then
              "  $ " ^ line
            else
              "  > " ^ line));
          cat_eof lines ~dest:user_shell_code_file;
          prln ". %s > %s 2>&1"
            (quote_for_sh user_shell_code_file_sh_path)
            (quote_for_sh user_shell_code_output_file_sh_path);
          prln {|%s -cwd %s -exit-code $? %s|}
            (quote_for_sh sanitizer_sh_path)
            (quote_for_sh cwd)
            (quote_for_sh user_shell_code_output_file);
          loop ()
    in
    loop ();
    let output_file = temp_file ".output" in
    let fd = Unix.openfile output_file [O_WRONLY; O_TRUNC] 0 in
    let pid =
      Unix.create_process "sh" [|"sh"; script|]
        Unix.stdin fd fd
    in
    Unix.close fd;
    let n =
      match snd (Unix.waitpid [] pid) with
      | WEXITED n -> n
      | _ -> 255
    in
    let output = Io.String_path.read_file output_file in
    if n <> 0 then begin
      Printf.eprintf "Generated cram script exited with code %d!\n" n;
      Printf.eprintf "Script:\n";
      let script = Io.String_path.read_file script in
      List.iter (String.split_lines script) ~f:(Printf.eprintf "| %s\n");
      Printf.eprintf "Script output:\n";
      List.iter (String.split_lines output) ~f:(Printf.eprintf "| %s\n");
      exit 1
    end;
    output)
}
