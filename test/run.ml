let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let log = Unix.openfile "log" [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let pid = Unix.create_process args.(0) args Unix.stdin log log in
  Unix.close log;
  match snd (Unix.waitpid [] pid) with
  | WEXITED 0 -> ()
  | st ->
    Printf.eprintf "Command failed, log saved in %s/log\n%!"
      (Sys.getcwd ());
    exit (match st with
      | WEXITED   n -> n
      | WSIGNALED n -> 128 + n
      | WSTOPPED  _ -> assert false)
