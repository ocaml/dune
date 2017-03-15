let inverse = ref false
let command_args = ref []
let logfile = ref "log"
let args =
  Arg.align
    [ "--", Rest (fun s -> command_args := s :: !command_args),
      " command to execute"
    ; "-inverse", Set inverse,
      " check that the command fail instead of checking that it succeed"
    ; "-log", Set_string logfile,
      "FILE set the log file"
    ]

let () =
  Arg.parse args
    (fun s -> raise (Arg.Bad (Printf.sprintf "Don't know what to do with %S" s)))
    "Usage: run [OPTIONS] -- ARGS";
  let command = Array.of_list (List.rev !command_args) in
  if Array.length command = 0 then (
    Printf.eprintf "Need at least a program to execute!\n";
    exit 2
  );
  let log = Unix.openfile !logfile [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let pid = Unix.create_process command.(0) command Unix.stdin log log in
  Unix.close log;
  match snd (Unix.waitpid [] pid) with
  | WEXITED 0 ->
    if !inverse then (
      Printf.eprintf "Command succeeded while it shouldn't, log saved in %s/%s\n%!"
        (Sys.getcwd ()) !logfile;
      exit 1
    )
  | WEXITED _ when !inverse -> ()
  | st ->
    Printf.eprintf "Command failed, log saved in %s/%s\n%!"
      (Sys.getcwd ()) !logfile;
    exit (match st with
      | WEXITED   n -> n
      | WSIGNALED n -> 128 + n
      | WSTOPPED  _ -> assert false)
