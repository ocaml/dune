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

let cram_stanzas lexbuf =
  let rec loop acc =
    match Cram_lexer.block lexbuf with
    | None -> List.rev acc
    | Some s -> loop (s :: acc)
  in
  loop []

let run_expect_test file ~f =
  let file_contents = Io.String_path.read_file file in
  let expected =
    let lexbuf = Lexbuf.from_string file_contents ~fname:file in
    f lexbuf
  in
  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then
    Io.String_path.write_file corrected_file expected
  else (
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0
  )

let extend_build_path_prefix_map ~env ~cwd =
  let var = "BUILD_PATH_PREFIX_MAP" in
  let s =
    Build_path_prefix_map.encode_map
      [ Some { source = cwd; target = "$TESTCASE_ROOT" } ]
  in
  Env.update env ~var ~f:(function
    | None -> Some s
    | Some s' -> Some (s ^ ":" ^ s'))

let fprln oc fmt = Printf.fprintf oc (fmt ^^ "\n")

(* Produce the following shell code:

   {v cat <<"EOF_<n>" <data> EOF_<n> v}

   choosing [<n>] such that [CRAM_EOF_<n>] doesn't appear in [<data>]. *)
let cat_eof ~dest oc lines =
  let prln fmt = fprln oc fmt in
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
  prln "cat >%s <<%S" (quote_for_sh dest) sentinel;
  List.iter lines ~f:(prln "%s");
  prln "%s" sentinel

type block_with_result =
  | Comment of string list
  | Command of
      { command : string list
      ; output_file : string
      }

type sh_script =
  { script : string
  ; cram_to_output : block_with_result list
  ; exit_codes_file : string
  }

(* Compose user written cram stanzas to output *)
let compose_cram_output (sh_script : sh_script) =
  let buf = Buffer.create 256 in
  let add_line line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  let add_line_prefixed_with_two_space line =
    Buffer.add_string buf "  ";
    add_line line
  in
  let next_code =
    let codes =
      Io.String_path.read_file ~binary:true sh_script.exit_codes_file
      |> String.split_lines
      |> List.map ~f:Int.of_string_exn
      |> ref
    in
    fun () ->
      match !codes with
      | [] -> None
      | x :: xs ->
        codes := xs;
        Some x
  in
  List.iter sh_script.cram_to_output ~f:(function
    | Comment lines -> List.iter lines ~f:add_line
    | Command { command; output_file } -> (
      List.iteri command ~f:(fun i line ->
          let line =
            sprintf "%c %s"
              ( if i = 0 then
                '$'
              else
                '>' )
              line
          in
          add_line_prefixed_with_two_space line);
      Io.String_path.read_file output_file
      |> String.split_lines
      |> List.iter ~f:add_line_prefixed_with_two_space;
      match next_code () with
      | None -> assert false
      | Some 0 -> ()
      | Some n -> add_line_prefixed_with_two_space (sprintf "[%d]" n) ));
  Buffer.contents buf

let create_sh_script cram_stanzas ~temp_dir ~sanitizer_command : sh_script =
  let script = Path.relative temp_dir "main.sh" in
  let oc = Io.open_out ~binary:true script in
  let script = Path.to_string script in
  let file name = Path.relative temp_dir name |> Path.to_absolute_filename in
  let sh_path path = quote_for_sh (translate_path_for_sh path) in
  let exit_codes_file = file ".exit_codes" in
  let exit_codes_file_sh_path = sh_path exit_codes_file in
  let loop i block =
    let i = succ i in
    match (block : Cram_lexer.block) with
    | Comment lines -> Comment lines
    | Command lines ->
      let file ~ext = file (sprintf "%d%s" i ext) in
      (* Shell code written by the user might not be properly terminated. For
         instance the user might forgot to write [EOF] after a [cat <<EOF]. If
         we wrote this shell code directly in the main script, it would hide the
         rest of the script. So instead, we dump each user written shell phrase
         into a file and then source it in the main script. *)
      let user_shell_code_file = file ~ext:".sh" in
      let user_shell_code_file_sh_path = sh_path user_shell_code_file in
      cat_eof oc lines ~dest:user_shell_code_file;
      (* Where we store the output of shell code written by the user *)
      let user_shell_code_output_file = file ~ext:".output" in
      let user_shell_code_output_file_sh_path =
        sh_path user_shell_code_output_file
      in
      let sanitized_output = file ~ext:".sanitized" in
      fprln oc ". %s > %s 2>&1" user_shell_code_file_sh_path
        user_shell_code_output_file_sh_path;
      fprln oc {|echo "$?" >> %s|} exit_codes_file_sh_path;
      let () =
        let sanitized_output = sh_path sanitized_output in
        (* XXX stderr outputted by the sanitizer command is ignored. That's not
           good. *)
        fprln oc {|%s < %s > %s|} sanitizer_command
          user_shell_code_output_file_sh_path sanitized_output
      in
      Command { command = lines; output_file = sanitized_output }
  in
  let cram_to_output = List.mapi ~f:loop cram_stanzas in
  close_out oc;
  { script; cram_to_output; exit_codes_file }

let display_with_bars s =
  List.iter (String.split_lines s) ~f:(Printf.eprintf "| %s\n")

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

  let temp_dir =
    let suffix = Filename.basename file in
    Temp.dir ~prefix:".dune.cram" ~suffix
  in
  let cram_stanzas = cram_stanzas lexbuf in
  let sh_script = create_sh_script cram_stanzas ~temp_dir ~sanitizer_command in
  Sys.chdir (Filename.dirname file);
  let cwd = Sys.getcwd () in
  let env =
    let env = Env.initial in
    let env = Env.add env ~var:"LC_ALL" ~value:"C" in
    let env = extend_build_path_prefix_map ~env ~cwd in
    let env =
      Env.add env ~var:"TMPDIR" ~value:(Path.to_absolute_filename temp_dir)
    in
    Env.to_unix env
  in
  let n =
    let pid =
      let null =
        if Sys.win32 then
          "nul"
        else
          "/dev/null"
      in
      let fd = Unix.openfile null [ O_WRONLY ] 0 in
      let pid =
        Unix.create_process_env "sh"
          [| "sh"; sh_script.script |]
          env Unix.stdin fd fd
      in
      Unix.close fd;
      pid
    in
    match Unix.waitpid [] pid with
    | _, WEXITED n -> n
    | _ -> 255
  in
  let output = compose_cram_output sh_script in
  if n <> 0 then (
    Printf.eprintf "Generated cram script exited with code %d!\n" n;
    Printf.eprintf "Script:\n";
    let script = Io.String_path.read_file sh_script.script in
    display_with_bars script;
    Printf.eprintf "Script output:\n";
    display_with_bars output;
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
