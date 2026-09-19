open Stdune
open Fiber.O
open Dune_scheduler

let child_count = 128

let config =
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let spawn () =
  Spawn.spawn
    ~prog:"/bin/sleep"
    ~argv0:"/bin/sleep"
    ~args:(Array.Immutable.of_list [ "10" ])
    ()
;;

let wait_for_process pid =
  let+ (_ : Proc.Process_info.t) =
    Scheduler.wait_for_process ~is_process_group_leader:false pid
  in
  ()
;;

let rec interrupt_when_registered children =
  if Scheduler.running_jobs_count (Scheduler.t ()) = child_count
  then (
    List.iter children ~f:(fun pid -> ignore (Pid.kill pid `Pid Term));
    Pid.kill_exn (Pid.me ()) `Pid Int;
    Fiber.return ())
  else
    let* () = Scheduler.sleep (Time.Span.of_secs 0.001) in
    interrupt_when_registered children
;;

let run_once () =
  match
    Scheduler.Run.go config ~timeout:(Time.Span.of_secs 2.) (fun () ->
      let children = List.init child_count ~f:(fun _ -> spawn ()) in
      Fiber.fork_and_join_unit
        (fun () -> Fiber.parallel_iter children ~f:wait_for_process)
        (fun () -> interrupt_when_registered children))
  with
  | () -> exit 2
  | exception Shutdown.E (Signal Int) -> ()
  | exception Shutdown.E _ -> exit 2
;;

let run () =
  for _ = 1 to 2 do
    run_once ()
  done
;;

let run_fresh () =
  let argv = [| Sys.executable_name; "run" |] in
  let dev_null = Unix.openfile "/dev/null" [ O_WRONLY ] 0 in
  let stderr, child_stderr = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process Sys.executable_name argv Unix.stdin dev_null child_stderr
  in
  Unix.close dev_null;
  Unix.close child_stderr;
  let stderr = Unix.in_channel_of_descr stderr in
  let output = In_channel.input_all stderr in
  close_in stderr;
  match snd (Unix.waitpid [] pid), String.is_empty output with
  | WEXITED 0, true -> ()
  | (WEXITED _ | WSIGNALED _ | WSTOPPED _), _ ->
    prerr_endline "signal watcher failed under concurrent SIGCHLD and SIGINT";
    exit 2
;;

let () =
  if Array.length Sys.argv = 1
  then
    for _ = 1 to 10 do
      run_fresh ()
    done
  else run ()
;;
