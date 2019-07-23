(* Mini implementation of cram tests *)

{
open Dune
open Import

type item =
  | Output  of string
  | Command of string list
  | Comment of string

let cwd = Sys.getcwd ()
}

let eol = '\n' | eof

let ext = '.' ['a'-'z' 'A'-'Z' '0'-'9']+

let abs_path = '/' ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_' '/']+

rule file = parse
 | eof { [] }
 | "  $ " { command [] lexbuf }
 | "  " { output lexbuf }
 | "" { comment lexbuf }

and comment = parse
 | [^'\n']* as str eol { Comment str :: file lexbuf }

and output = parse
  | [^'\n']* as str eol { Output str :: file lexbuf }

and command acc = parse
 | ([^'\n']* as str) "\n  > "
    { command (str :: acc) lexbuf }
 | ([^'\n']* as str) eol
    { Command (List.rev (str :: acc)) :: file lexbuf }

and postprocess_cwd b = parse
  | eof { Buffer.contents b }
  | (abs_path as path) {
      let path =
        match String.drop_prefix path ~prefix:cwd with
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

  let () =
    let skip_versions = ref [] in
    let expect_test = ref None in
    let args =
      [ "-skip-versions"
      , Arg.String (fun s -> skip_versions := parse_skip_versions s)
      , "Comma separated versions of ocaml where to skip test"
      ; "-test"
      , Arg.String (fun s -> expect_test := Some s)
      , "expect test file"
      ] in
    Configurator.main ~args ~name:"cram" (fun configurator ->
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
      let env =
        Env.add Env.initial ~var:"LC_ALL" ~value:"C"
        |> Env.to_unix
      in
      Test_common.run_expect_test expect_test ~f:(fun file_contents lexbuf ->
        let items = file lexbuf in
        let temp_file = Filename.temp_file "dune-test" ".output" in
        at_exit (fun () -> Sys.remove temp_file);
        let buf = Buffer.create (String.length file_contents + 1024) in
        List.iter items ~f:(function
          | Output _ -> ()
          | Comment s -> Buffer.add_string buf s; Buffer.add_char buf '\n'
          | Command l ->
            Printf.bprintf buf "  $ %s\n" (String.concat l ~sep:"\n  > ");
            let s = String.concat l ~sep:"\n" in
            let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
            let pid =
              Unix.create_process_env "sh" [|"sh"; "-c"; s|]
                env Unix.stdin fd fd
            in
            Unix.close fd;
            let n =
              match snd (Unix.waitpid [] pid) with
              | WEXITED n -> n
              | _ -> 255
            in
            let ext_replace = make_ext_replace configurator in
            Path.of_filename_relative_to_initial_cwd temp_file
            |> Io.lines_of_file
            |> List.iter ~f:(fun line ->
              let line =
                line
                |> Ansi_color.strip
                |> ext_replace
                |> cwd_replace
                |> config_var_replace configurator
              in
              Printf.bprintf buf "  %s\n" line);
            if n <> 0 then Printf.bprintf buf "  [%d]\n" n);
        Buffer.contents buf)
    )
}
