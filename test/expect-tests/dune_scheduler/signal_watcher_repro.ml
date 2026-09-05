open Stdune
module Event = Dune_scheduler__Event
module Signal_watcher = Dune_scheduler__Signal_watcher
module Thread0 = Dune_scheduler__Thread0

let spawn () =
  Unix.create_process
    "/bin/sh"
    [| "/bin/sh"; "-c"; "sleep 0.01" |]
    Unix.stdin
    Unix.stdout
    Unix.stderr
;;

let rec reap () =
  match Unix.waitpid [ WNOHANG ] (-1) with
  | 0, _ -> ()
  | _, _ -> reap ()
  | exception Unix.Unix_error (ECHILD, _, _) -> ()
;;

let rec wait_for_sigint events =
  match Event.Queue.next events with
  | Shutdown (Signal Int) -> ()
  | Shutdown _ | Fiber_fill_ivar _ -> wait_for_sigint events
  | Job_complete_ready ->
    reap ();
    wait_for_sigint events
;;

let interrupt () =
  Thread.create
    (fun () ->
       Thread.delay 0.002;
       Unix.kill (Unix.getpid ()) Sys.sigint)
    ()
;;

let run () =
  Thread0.interrupt_signals
  |> List.map ~f:Signal.to_int
  |> Unix.sigprocmask SIG_UNBLOCK
  |> ignore;
  let events = Event.Queue.create () in
  let signal_watcher = Signal_watcher.init ~print_ctrl_c_warning:false events in
  let finished = Atomic.make false in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
         Thread.delay 1.;
         if not (Atomic.get finished) then exit 2)
      ()
  in
  for _ = 1 to 2 do
    let children = List.init 128 ~f:(fun _ -> spawn ()) in
    let interrupt = interrupt () in
    wait_for_sigint events;
    Thread.join interrupt;
    List.iter children ~f:(fun pid ->
      try ignore (Unix.waitpid [] pid : int * Unix.process_status) with
      | Unix.Unix_error (ECHILD, _, _) -> ())
  done;
  Unix.kill (Unix.getpid ()) (Signal.to_int Thread0.signal_watcher_interrupt);
  Thread0.join signal_watcher;
  Atomic.set finished true
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
