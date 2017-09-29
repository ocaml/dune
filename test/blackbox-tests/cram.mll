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
    Test_common.run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
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
            Printf.bprintf buf "  %s\n" line);
          if n <> 0 then Printf.bprintf buf "  [%d]\n" n);
      Buffer.contents buf)
}


