(* Mini implementation of cram tests *)

{
open Jbuilder
open Import

type item =
  | Output  of string
  | Command of string
  | Comment of string
}

let eol = '\n' | eof

rule file = parse
 | eof { [] }
 | "  $ " ([^'\n']* as str) eol { Command str :: file lexbuf }
 | "  " ([^'\n']* as str) eol   { Output  str :: file lexbuf }
 | ([^'\n']* as str) eol        { Comment str :: file lexbuf }

{
  let () =
    let ocaml_version = ref None in
    let skip_versions = ref [] in
    let expect_test = ref None in
    let usage = sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name) in
    let anon s =
      match !expect_test with
      | None -> expect_test := Some s
      | Some _ -> raise (Arg.Bad "test must only be given once") in
    Arg.parse
      [ "-ocamlv"
      , Arg.String (fun s -> ocaml_version := Some s)
      , "Version of ocaml being used"
      ; "-skip-versions"
      , Arg.String (fun s -> skip_versions := String.split s ~on:',')
      , "Comma separated versions of ocaml where to skip test"
      ] anon usage;
    let expect_test =
      match !expect_test with
      | None -> raise (Arg.Bad "expect test file must be passed")
      | Some p -> p in
    begin match !ocaml_version, !skip_versions with
    | None, [] -> ()
    | None, _::_ -> raise (Arg.Bad "provide -ocaml along with -skip-versions")
    | Some v, skip -> if List.mem v ~set:skip then exit 0
    end;
    Test_common.run_expect_test expect_test ~f:(fun file_contents lexbuf ->
      let items = file lexbuf in
      let temp_file = Filename.temp_file "jbuilder-test" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (String.length file_contents + 1024) in
      List.iter items ~f:(function
        | Output _ -> ()
        | Comment s -> Buffer.add_string buf s; Buffer.add_char buf '\n'
        | Command s ->
          Printf.bprintf buf "  $ %s\n" s;
          let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
          let pid =
            Unix.create_process "sh" [|"sh"; "-c"; s|] Unix.stdin fd fd
          in
          Unix.close fd;
          let n =
            match snd (Unix.waitpid [] pid) with
            | WEXITED n -> n
            | _ -> 255
          in
          List.iter (Io.lines_of_file temp_file) ~f:(fun line ->
            Printf.bprintf buf "  %s\n" (Ansi_color.strip line));
          if n <> 0 then Printf.bprintf buf "  [%d]\n" n);
      Buffer.contents buf)
}


