module Exn = Stdune.Exn
module Pid = Stdune.Pid
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

let%expect_test "sigprocmask" =
  if not Sys.win32
  then (
    let run ?sigprocmask expected_signal =
      let prog = Program_lookup.find_prog "sleep" in
      let pid = Spawn.spawn ?sigprocmask ~prog ~argv:[ "sleep"; "60" ] () in
      let pid_int = Pid.to_int pid in
      Unix.kill pid_int Sys.sigusr1;
      Unix.kill pid_int Sys.sigkill;
      match Unix.waitpid [] pid_int with
      | _, WSIGNALED signal when signal = expected_signal -> ()
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
