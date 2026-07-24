module Int = Stdune.Int
module Exn = Stdune.Exn
module Pid = Stdune.Pid
module Proc = Stdune.Proc
module Platform = Stdune.Platform
module Signal = Stdune.Signal
module Spawn = Stdune.Spawn

let show_raise f =
  try ignore (f ()) with
  | exn ->
    let s =
      match exn with
      | Unix.Unix_error _ ->
        (* For compat with Windows *)
        "Unix.Unix_error _"
      | exn -> Printexc.to_string exn
    in
    Printf.printf "raised %s" s
;;

let%expect_test "non-existing program" =
  show_raise (fun () -> Spawn.spawn () ~prog:"/doesnt-exist" ~argv:[ "blah" ]);
  [%expect
    {|
    raised Unix.Unix_error _
  |}]
;;

let%expect_test "exec failure returns if standard descriptors are closed" =
  if Platform.OS.value = Linux
  then
    assert (
      match Unix.fork () with
      | 0 ->
        let dev_null = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
        Unix.close Unix.stdin;
        Unix.close Unix.stdout;
        (match
           Spawn.spawn
             ()
             ~prog:"/doesnt-exist"
             ~argv:[ "blah" ]
             ~stdin:dev_null
             ~stdout:dev_null
             ~stderr:dev_null
         with
         | pid ->
           (match Proc.wait (Pid pid) [] with
            | Some _ -> Unix._exit 0
            | None -> Unix._exit 2)
         | exception Unix.Unix_error _ -> Unix._exit 1)
      | child ->
        (match Proc.wait (Pid (Pid.of_int_exn child)) [] with
         | Some { status = WEXITED 0; _ } -> true
         | Some _ | None -> false));
  [%expect {||}]
;;

let%expect_test "non-existing dir" =
  show_raise (fun () ->
    Spawn.spawn
      ()
      ~prog:"/bin/true"
      ~argv:[ "true" ]
      ~cwd:(Spawn.Working_dir.Path "/doesnt-exist"));
  [%expect
    {|
    raised Unix.Unix_error _
  |}]
;;

let wait pid =
  match snd (Unix.waitpid [] (Pid.to_int pid)) with
  | WEXITED 0 -> ()
  | WEXITED n -> Printf.ksprintf failwith "exited with code %d" n
  | WSIGNALED n -> Printf.ksprintf failwith "got signal %d" n
  | WSTOPPED n -> Printf.ksprintf failwith "stopped with signal %d" n
;;

let list_files = Filename.concat (Sys.getcwd ()) "exe/list_files.exe"
let print_env = Filename.concat (Sys.getcwd ()) "exe/print_env.exe"

let () =
  Unix.mkdir "sub" 0o777;
  close_out (open_out "sub/foo");
  close_out (open_out "sub/bar")
;;

let%expect_test "cwd:Path" =
  wait
    (Spawn.spawn
       ()
       ~prog:list_files
       ~argv:[ "list_files.exe" ]
       ~cwd:(Spawn.Working_dir.Path "sub"));
  [%expect
    {|
    bar
    foo
  |}]
;;

let%expect_test "cwd:Fd" =
  if Sys.win32
  then print_endline "bar\nfoo"
  else (
    let fd = Unix.openfile "sub" [ O_RDONLY ] 0 in
    wait
      (Spawn.spawn
         ()
         ~prog:list_files
         ~argv:[ "list_files.exe" ]
         ~cwd:(Spawn.Working_dir.Fd fd));
    Unix.close fd);
  [%expect
    {|
    bar
    foo
  |}]
;;

let%expect_test "cwd:Fd (invalid)" =
  show_raise (fun () ->
    if Sys.win32
    then raise (Unix.Unix_error (ENOTDIR, "fchdir", ""))
    else
      Spawn.spawn
        ()
        ~prog:"/bin/pwd"
        ~argv:[ "pwd" ]
        ~cwd:(Spawn.Working_dir.Fd Unix.stdin));
  [%expect
    {|
    raised Unix.Unix_error _
  |}]
;;

module Program_lookup = struct
  let path_sep = if Sys.win32 then ';' else ':'
  let exe_ext = if Sys.win32 then ".exe" else ""

  let split_path s =
    let rec loop i j =
      if j = String.length s
      then [ String.sub s i (j - i) ]
      else if s.[j] = path_sep
      then String.sub s i (j - i) :: loop (j + 1) (j + 1)
      else loop i (j + 1)
    in
    loop 0 0
  ;;

  let path =
    match Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> split_path s
  ;;

  let find_prog prog =
    let rec search = function
      | [] -> Printf.ksprintf failwith "Program %S not found in PATH!" prog
      | dir :: rest ->
        let fn = Filename.concat dir prog ^ exe_ext in
        if Sys.file_exists fn then fn else search rest
    in
    search path
  ;;
end

let%expect_test "inheriting stdout with close-on-exec set" =
  (* CR-soon jdimino for jdimino: the test itself seems to pass, however there
     seem to be another issue related to ppx_expect and Windows. *)
  if Sys.win32
  then print_string "hello world"
  else (
    Unix.set_close_on_exec Unix.stdout;
    let shell, arg = if Sys.win32 then "cmd", "/c" else "sh", "-c" in
    let prog = Program_lookup.find_prog shell in
    wait (Spawn.spawn () ~prog ~argv:[ shell; arg; {|echo "hello world"|} ]));
  [%expect {| hello world |}]
;;

let%expect_test "prog relative to cwd" =
  if Sys.win32
  then print_string "Hello, world!"
  else
    wait
      (Spawn.spawn
         ()
         ~prog:"./hello.exe"
         ~argv:[ "hello" ]
         ~cwd:(Spawn.Working_dir.Path "exe"));
  [%expect {| Hello, world! |}]
;;

let%expect_test "env" =
  let tst v =
    let env =
      match v with
      | None -> Spawn.Env.of_list []
      | Some v -> Spawn.Env.of_list [ "FOO=" ^ v ]
    in
    wait
      (Spawn.spawn
         ()
         ~env
         ~prog:"./print_env.exe"
         ~argv:[ "print_env" ]
         ~cwd:(Spawn.Working_dir.Path "exe"))
  in
  tst (Some "foo");
  [%expect {| Some "foo" |}];
  tst None;
  [%expect {| None |}];
  tst (Some "");
  [%expect {| Some "" |}]
;;

let%expect_test "pgid tests" =
  wait
    (Spawn.spawn
       ~setpgid:Spawn.Pgid.new_process_group
       ()
       ~prog:"pgid_test/checkpgid.exe"
       ~argv:[]);
  [%expect {||}]
;;

let wait_for_file ?(timeout = 5.0) fn =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Sys.file_exists fn
    then ()
    else if Unix.gettimeofday () >= deadline
    then Printf.ksprintf failwith "timed out waiting for %s" fn
    else (
      ignore (Unix.select [] [] [] 0.05);
      loop ())
  in
  loop ()
;;

let wait_for_no_file ?(timeout = 1.0) fn =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Sys.file_exists fn
    then Printf.ksprintf failwith "%s unexpectedly exists" fn
    else if Unix.gettimeofday () >= deadline
    then ()
    else (
      ignore (Unix.select [] [] [] 0.05);
      loop ())
  in
  loop ()
;;

let kill_child pid_file =
  if Sys.file_exists pid_file
  then (
    let pid_file_contents = Stdune.Io.String_path.read_file pid_file in
    match Int.of_string (String.trim pid_file_contents) with
    | None -> ()
    | Some pid ->
      let pid = Pid.of_int_exn pid in
      (match Pid.kill pid `Pid Kill with
       | `Delivered | `Dead -> ()))
;;

let with_pdeathsig_child mode ~spawn ~f =
  let temp_name suffix =
    Filename.concat
      (Sys.getcwd ())
      (Printf.sprintf "stdune-spawn-%d-%s-%s" (Unix.getpid ()) mode suffix)
  in
  let ready_file = temp_name "ready" in
  let marker_file = temp_name "marker" in
  let pid_file = temp_name "pid" in
  let cleanup () =
    kill_child pid_file;
    List.iter
      (fun fn -> if Sys.file_exists fn then Unix.unlink fn)
      [ ready_file; marker_file; pid_file ]
  in
  Exn.protect ~finally:cleanup ~f:(fun () ->
    cleanup ();
    match Unix.fork () with
    | 0 ->
      let exit_code =
        try
          let argv = "print_env" :: mode :: [ ready_file; marker_file ] in
          let child = spawn argv in
          Stdune.Io.String_path.write_file
            pid_file
            (Printf.sprintf "%d\n" (Pid.to_int child));
          wait_for_file ready_file;
          0
        with
        | _ -> 1
      in
      Unix._exit exit_code
    | intermediate ->
      wait (Pid.of_int_exn intermediate);
      f ~marker_file)
;;

let%expect_test "pdeathsig defaults to kill" =
  (match Platform.OS.value with
   | Linux ->
     with_pdeathsig_child
       "pdeathsig-default-child"
       ~f:(fun ~marker_file -> wait_for_no_file marker_file)
       ~spawn:(fun argv -> Spawn.spawn () ~prog:print_env ~argv)
   | _ -> ());
  [%expect {||}]
;;

let%expect_test "pdeathsig explicit signal" =
  (match Platform.OS.value with
   | Linux ->
     with_pdeathsig_child
       "pdeathsig-child"
       ~f:(fun ~marker_file -> wait_for_file marker_file)
       ~spawn:(fun argv -> Spawn.spawn ~pdeathsig:Usr1 () ~prog:print_env ~argv)
   | _ -> ());
  [%expect {||}]
;;

let%expect_test "sigprocmask" =
  if not Sys.win32
  then (
    let run ?sigprocmask expected_signal =
      let prog = Program_lookup.find_prog "sleep" in
      let pid = Spawn.spawn ?sigprocmask ~prog ~argv:[ "sleep"; "60" ] () in
      let pid_int = Pid.to_int pid in
      Unix.kill pid_int Sys.sigusr1;
      Unix.kill pid_int Sys.sigkill;
      match Proc.wait (Pid pid) [] with
      | Some { status = WSIGNALED signal; _ } when signal = expected_signal -> ()
      | _ -> failwith "unexpected"
    in
    run Sys.sigusr1;
    run ~sigprocmask:(SIG_BLOCK, [ Sys.sigusr1 ]) Sys.sigkill;
    let old_signals = Unix.sigprocmask SIG_BLOCK [ Sys.sigusr1 ] in
    Exn.protect
      ~finally:(fun () -> ignore (Unix.sigprocmask SIG_SETMASK old_signals : int list))
      ~f:(fun () ->
        (* The blocking of [sigusr1] is only propagated to the child process if
           the sigprocmask is [SIG_BLOCK] or [SIG_UNBLOCK]. *)
        run Sys.sigusr1;
        run ~sigprocmask:(SIG_BLOCK, []) Sys.sigkill;
        run ~sigprocmask:(SIG_UNBLOCK, []) Sys.sigkill;
        (* Unblocking sigusr1 in the child process. *)
        run ~sigprocmask:(SIG_UNBLOCK, [ Sys.sigusr1 ]) Sys.sigusr1;
        run ~sigprocmask:(SIG_SETMASK, []) Sys.sigusr1));
  [%expect {||}]
;;

(* This should be at the end to clean up the test environment *)
let () =
  Unix.unlink "sub/foo";
  Unix.unlink "sub/bar";
  Unix.rmdir "sub"
;;
