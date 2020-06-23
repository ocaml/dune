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

let _BUILD_PATH_PREFIX_MAP = "BUILD_PATH_PREFIX_MAP"

let extend_build_path_prefix_map ~env ~cwd =
  let s =
    Build_path_prefix_map.encode_map
      [ Some { source = cwd; target = "$TESTCASE_ROOT" } ]
  in
  Env.update env ~var:_BUILD_PATH_PREFIX_MAP ~f:(function
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

type block_result =
  { command : string list
  ; output_file : string
  }

type metadata_entry =
  { exit_code : int
  ; build_path_prefix_map : string
  }

type full_block_result = block_result * metadata_entry

type sh_script =
  { script : string
  ; cram_to_output : block_result Cram_lexer.block list
  ; metadata_file : string
  }

let read_exit_codes_and_prefix_maps file =
  let s = Io.String_path.read_file ~binary:true file in
  let rec loop acc = function
    | exit_code :: build_path_prefix_map :: entries ->
      let exit_code =
        match Int.of_string exit_code with
        | Some s -> s
        | None ->
          Code_error.raise "invalid metadata file"
            [ ("entries", Dyn.Encoder.string s)
            ; ("exit_code", Dyn.Encoder.string exit_code)
            ]
      in
      loop ({ exit_code; build_path_prefix_map } :: acc) entries
    | [ "" ]
    | [] ->
      List.rev acc
    | [ _ ] ->
      Code_error.raise "odd number of elements" [ ("s", Dyn.Encoder.string s) ]
  in
  loop [] (String.split ~on:'\000' s)

let read_and_attach_exit_codes (sh_script : sh_script) :
    full_block_result Cram_lexer.block list =
  let metadata_entries =
    read_exit_codes_and_prefix_maps sh_script.metadata_file
  in
  let rec loop acc entries blocks =
    match (blocks, entries) with
    | [], [] -> List.rev acc
    | (Cram_lexer.Comment _ as comment) :: blocks, _ ->
      loop (comment :: acc) entries blocks
    | Command block_result :: blocks, metadata_entry :: entries ->
      loop (Command (block_result, metadata_entry) :: acc) entries blocks
    | Cram_lexer.Command _ :: _, [] ->
      Code_error.raise "command without metadata" []
    | [], _ :: _ -> Code_error.raise "more blocks than metadata" []
  in
  loop [] metadata_entries sh_script.cram_to_output

let sanitize ~temp_dir ~prog ~argv cram_to_output =
  let sanitized_outputs =
    let commands : Sanitizer.Command.t list =
      List.filter_map cram_to_output ~f:(function
        | Cram_lexer.Comment _ -> None
        | Command (block_result, { build_path_prefix_map; exit_code = _ }) ->
          let output = Io.String_path.read_file block_result.output_file in
          Some { Sanitizer.Command.output; build_path_prefix_map })
    in
    Sanitizer.run_sanitizer ~temp_dir ~prog ~argv commands
  in
  let rec loop acc blocks outputs =
    match (blocks, outputs) with
    | [], [] -> List.rev acc
    | (Cram_lexer.Comment _ as comment) :: blocks, _ ->
      loop (comment :: acc) blocks outputs
    | Command (result, entry) :: blocks, output :: outputs ->
      loop (Command (result, entry, output) :: acc) blocks outputs
    | Command _ :: _, [] -> assert false
    | [], _ :: _ -> assert false
  in
  loop [] cram_to_output sanitized_outputs

(* Compose user written cram stanzas to output *)
let compose_cram_output (cram_to_output : _ Cram_lexer.block list) =
  let buf = Buffer.create 256 in
  let add_line line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  let add_line_prefixed_with_two_space line =
    Buffer.add_string buf "  ";
    add_line line
  in
  List.iter cram_to_output ~f:(fun block ->
      match (block : _ Cram_lexer.block) with
      | Comment lines -> List.iter lines ~f:add_line
      | Command
          ( { command; output_file = _ }
          , { exit_code; build_path_prefix_map = _ }
          , output ) -> (
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
        String.split_lines output
        |> List.iter ~f:add_line_prefixed_with_two_space;
        match exit_code with
        | 0 -> ()
        | n -> add_line_prefixed_with_two_space (sprintf "[%d]" n) ));
  Buffer.contents buf

let create_sh_script cram_stanzas ~temp_dir : sh_script =
  let script = Path.relative temp_dir "main.sh" in
  let oc = Io.open_out ~binary:true script in
  let script = Path.to_string script in
  let file name = Path.relative temp_dir name |> Path.to_absolute_filename in
  let sh_path path = quote_for_sh (translate_path_for_sh path) in
  let metadata_file = file "cram.metadata" in
  let metadata_file_sh_path = sh_path metadata_file in
  let loop i block =
    let i = succ i in
    match (block : _ Cram_lexer.block) with
    | Comment _ as comment -> comment
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
      fprln oc ". %s > %s 2>&1" user_shell_code_file_sh_path
        user_shell_code_output_file_sh_path;
      fprln oc {|printf "$?\0$%s\0" >> %s|} _BUILD_PATH_PREFIX_MAP
        metadata_file_sh_path;
      Command { command = lines; output_file = user_shell_code_output_file }
  in
  let cram_to_output = List.mapi ~f:loop cram_stanzas in
  close_out oc;
  { script; cram_to_output; metadata_file }

let display_with_bars s =
  List.iter (String.split_lines s) ~f:(Printf.eprintf "| %s\n")

let run ~sanitizer ~file lexbuf =
  let temp_dir =
    let suffix = Filename.basename file in
    Temp.dir ~prefix:"dune.cram." ~suffix
  in
  let cram_stanzas = cram_stanzas lexbuf in
  let sh_script = create_sh_script cram_stanzas ~temp_dir in
  Sys.chdir (Filename.dirname file);
  let cwd = Sys.getcwd () in
  let env =
    let env = Env.initial in
    let env = Env.add env ~var:"LC_ALL" ~value:"C" in
    let env = extend_build_path_prefix_map ~env ~cwd in
    let env =
      Env.add env ~var:Env.Var.temp_dir
        ~value:(Path.to_absolute_filename temp_dir)
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
  let output =
    let raw = read_and_attach_exit_codes sh_script in
    let sanitized =
      let prog, argv =
        match sanitizer with
        | None -> (Sys.executable_name, [ "sanitize" ])
        | Some prog ->
          let prog =
            if Filename.is_relative prog then
              Filename.concat (Sys.getcwd ()) prog
            else
              prog
          in
          (prog, [])
      in
      sanitize ~temp_dir ~prog ~argv raw
    in
    compose_cram_output sanitized
  in
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
