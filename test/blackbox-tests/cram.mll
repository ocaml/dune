(* Mini implementation of cram tests *)

{
open Dune
open Import

type dot_t_block =
  | Command of string list
  | Comment of string list

let cwd = ref (Sys.getcwd ())

(* Translate a path for [sh]. On Windows, [sh] will come from Cygwin
   so if we are a real windows program we need to pass the path
   through [cygpath] *)
let translate_path_for_sh =
  if not Sys.win32 then
    fun _ fn -> fn
  else
    fun cfg fn ->
      Configurator.V1.Process.run_capture_exn cfg
        "cygpath" ["cygpath"; fn]
      |> String.split_lines
      |> List.hd_opt
      |> Option.value ~default:""

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

let ext = '.' ['a'-'z' 'A'-'Z' '0'-'9']+

let abs_path = '/' ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_' '/']+

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

and postprocess_cwd b = parse
  | eof { Buffer.contents b }
  | (abs_path as path) {
      let path =
        match String.drop_prefix path ~prefix:!cwd with
        | None -> path
        | Some path -> "$TESTCASE_ROOT" ^ path
      in
      Buffer.add_string b path; postprocess_cwd b lexbuf
    }
  | _ as c { Buffer.add_char b c; postprocess_cwd b lexbuf }

and postprocess_ext tbl b = parse
  | eof { Buffer.contents b }
  | ([^ '/'] as c) (ext as e)
      { Buffer.add_char b c;
        begin match List.assoc tbl e with
        | Some res -> Buffer.add_string b res
        | None     -> Buffer.add_string b e
        end;
        postprocess_ext tbl b lexbuf
      }
  | _ as c { Buffer.add_char b c; postprocess_ext tbl b lexbuf }

{
  module Configurator = Configurator.V1

  let cwd_replace s =
    let l = Lexing.from_string s in
    postprocess_cwd (Buffer.create (String.length s)) l

  let remove_std_arg =
    let spacem, space = Re.mark (Re.rep Re.space) in
    let re =
      [ space
      ; Re.str "-std="
      ; Re.rep1 (Re.compl [Re.space])
      ; space
      ]
      |> Re.seq
      |> Re.compile
    in
    fun s -> Re.replace re s ~f:(fun g ->
      if Re.Mark.test g spacem then
        " "
      else
        "")

  let config_var_replace config : string -> string =
    let vars =
      [ "ocamlc_cflags"
      ; "ocamlc_cppflags"
      ]
    in
    let vars =
      List.filter_map vars ~f:(fun k ->
        Configurator.ocaml_config_var config k
        |> Option.map ~f:(fun v -> (k, v)))
    in
    (* we filter out stuff like -std=gnu99 to get the list of C++ flags *)
    let vars =
      match List.assoc vars "ocamlc_cflags" with
      | None -> vars
      | Some v -> ("cxx_flags", remove_std_arg v) :: vars
    in
    let values =
      List.filter_map vars ~f:(fun (_, v) ->
        match String.trim v with
        | "" -> None
        | v -> Some (Re.str v))
    in
    let re = Re.compile (Re.alt values) in
    fun s -> Re.replace re ~f:(fun _ -> "$flags") s

  let make_ext_replace config =
    let tbl =
      let var = Configurator.ocaml_config_var_exn config in
      let exts =
      [ var "ext_dll", "$ext_dll"
      ; var "ext_asm", "$ext_asm"
      ; var "ext_lib", "$ext_lib"
      ; var "ext_obj", "$ext_obj"
      ] in
      (* need to special case exe since we can only remove this extension in
         general *)
      match (
        match Configurator.ocaml_config_var config "ext_exe" with
        | Some s -> s
        | None ->
          begin match Configurator.ocaml_config_var_exn config "system" with
          | "Win32" -> ".exe"
          | _ -> ""
          end
      ) with
      | "" -> exts
      | ext -> (ext, "") :: exts
    in
    List.iter tbl ~f:(fun (e, _) -> assert (e <> ""));
    fun s ->
      let l = Lexing.from_string s in
      postprocess_ext tbl (Buffer.create (String.length s)) l

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
    let skip_versions = ref [] in
    let expect_test = ref None in
    let exit_code = ref 0 in
    let sanitize = ref None in
    let args =
      [ "-skip-versions"
      , Arg.String (fun s -> skip_versions := parse_skip_versions s)
      , "VERSION Comma separated versions of ocaml where to skip test"
      ; "-test"
      , Arg.String (fun s -> expect_test := Some s)
      , "FILE expect test file"
      ; "-sanitize"
      , Arg.String (fun s -> sanitize := Some s)
      , "FILE Sanitize the command output contained in this file"
      ; "-exit-code"
      , Arg.Set_int exit_code
      , "NUMBER Exit code of the command for which we are sanitizing the output"
      ; "-cwd"
      , Arg.Set_string cwd
      , "DIR Set the cwd for sanitizing the command output"
      ]
    in
    Configurator.main ~args ~name:"cram" (fun configurator ->
      Option.iter !sanitize ~f:(fun fn ->
        let sanitize_line =
          let ext_replace = make_ext_replace configurator in
          fun line ->
            line
            |> Ansi_color.strip
            |> ext_replace
            |> cwd_replace
            |> config_var_replace configurator
        in
        List.iter (Io.String_path.lines_of_file fn) ~f:(fun line ->
          Printf.printf "  %s\n" (sanitize_line line));
        if !exit_code <> 0 then Printf.printf "  [%d]\n" !exit_code;
        exit 0);
      let expect_test =
        match !expect_test with
        | None -> raise (Arg.Bad "expect test file must be passed")
        | Some p -> p in
      begin
        let ocaml_version =
          Configurator.ocaml_config_var_exn configurator "version"
          |> parse_version in
        if List.exists !skip_versions ~f:(fun (op, v') ->
          test op ocaml_version v') then
          exit 0;
      end;
      Unix.putenv "LC_ALL" "C";
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
          translate_path_for_sh configurator user_shell_code_file
        in
        (* Where we store the output of shell code written by the user *)
        let user_shell_code_output_file = temp_file ".output" in
        let user_shell_code_output_file_sh_path =
          translate_path_for_sh configurator user_shell_code_output_file
        in
        let executable_name_sh_path =
          translate_path_for_sh configurator Sys.executable_name
        in
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
              prln {|%s -cwd %s -exit-code $? -sanitize %s|}
                (quote_for_sh executable_name_sh_path)
                (quote_for_sh !cwd)
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
          Printf.eprintf "Script output:";
          List.iter (String.split_lines output) ~f:(Printf.eprintf "| %s\n");
          exit 1
        end;
        output))
}
