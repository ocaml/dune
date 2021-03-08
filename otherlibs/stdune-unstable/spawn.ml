(** Note that this function's behavior differs between windows and unix.

    - [Unix.create_process{,_env} prog] looks up prog in PATH
    - [Unix.execv{_,e} does not look up prog in PATH] *)
let spawn ?env ~prog ~argv ?(stdin = Unix.stdin) ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr) () =
  let argv = Array.of_list argv in
  let env = Option.map ~f:Env.to_unix env in
  Pid.of_int
    ( match env with
    | None -> Unix.create_process prog argv stdin stdout stderr
    | Some env -> Unix.create_process_env prog argv env stdin stdout stderr )
