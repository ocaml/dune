open Stdune
open Fiber.O
open Dune_scheduler

let config =
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let string_contains s needle =
  let len_s = String.length s in
  let len_needle = String.length needle in
  let rec loop i =
    i + len_needle <= len_s
    && (String.sub s ~pos:i ~len:len_needle = needle || loop (i + 1))
  in
  loop 0
;;

let print_relevant_stderr stderr =
  String.split_lines stderr
  |> List.filter ~f:(fun line ->
    string_contains line "signal watcher received an unexpected signal")
  |> List.iter ~f:print_endline
;;

let signal_cleanup_repro_prog =
  let inline_test_dir = Filename.dirname Sys.executable_name in
  let build_dir = Filename.dirname inline_test_dir in
  let prog = Filename.concat build_dir "signal_cleanup_repro.exe" in
  if Sys.file_exists prog
  then prog
  else
    Code_error.raise
      "could not locate signal cleanup repro executable"
      [ "cwd", Dyn.string (Sys.getcwd ())
      ; "test_executable", Dyn.string Sys.executable_name
      ; "expected", Dyn.string prog
      ]
;;

let signal_cleanup_repro_env () =
  let env =
    Env.initial
    |> Env.add ~var:"DUNE_SIGNAL_REPRO_SHUTDOWN" ~value:"signal"
    |> Env.add ~var:"DUNE_SIGNAL_REPRO_ITERS" ~value:"1"
    |> Env.add ~var:"DUNE_SIGNAL_REPRO_JOBS" ~value:"128"
    |> Env.add ~var:"DUNE_SIGNAL_REPRO_DELAY" ~value:"0.002"
  in
  Env.to_unix env |> Array.of_list
;;

let run_signal_cleanup_repro () =
  let ic, oc, ec =
    Unix.open_process_args_full
      signal_cleanup_repro_prog
      [| signal_cleanup_repro_prog |]
      (signal_cleanup_repro_env ())
  in
  close_out oc;
  let stdout = In_channel.input_all ic in
  let stderr = In_channel.input_all ec in
  match Unix.close_process_full (ic, oc, ec) with
  | WEXITED 0 ->
    let stdout = String.trim stdout in
    if String.equal stdout "ok after 1 iterations"
    then true
    else (
      ignore stdout;
      print_relevant_stderr stderr;
      false)
  | status ->
    ignore stdout;
    ignore status;
    print_relevant_stderr stderr;
    false
;;

let%expect_test "read readiness" =
  (Scheduler.Run.go config ~on_event:(fun _ _ -> ())
   @@ fun () ->
   let r, w = Unix.pipe ~cloexec:true () in
   if not Sys.win32 then Unix.set_nonblock r;
   let* task = Async_io.ready r `Read ~f:ignore in
   assert (Unix.write w (Bytes.of_string "0") 0 1 = 1);
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     let bytes = Bytes.of_string "1" in
     assert (Unix.read r bytes 0 1 = 1);
     assert (Bytes.to_string bytes = "0");
     Unix.close w;
     let+ () = Async_io.close r in
     print_endline "successful read");
  [%expect {| successful read |}]
;;

let%expect_test "write readiness" =
  (Scheduler.Run.go config ~on_event:(fun _ _ -> ())
   @@ fun () ->
   let r, w = Unix.pipe ~cloexec:true () in
   if not Sys.win32 then Unix.set_nonblock w;
   let* task = Async_io.ready w `Write ~f:ignore in
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     assert (Unix.write w (Bytes.of_string "0") 0 1 = 1);
     Unix.close r;
     let+ () = Async_io.close w in
     print_endline "successful write");
  [%expect {| successful write |}]
;;

let%expect_test "first ready" =
  (Scheduler.Run.go config ~on_event:(fun _ _ -> ())
   @@ fun () ->
   let r1, w1 = Unix.pipe ~cloexec:true () in
   let r2, w2 = Unix.pipe ~cloexec:true () in
   if not Sys.win32
   then (
     Unix.set_nonblock w1;
     Unix.set_nonblock w2);
   let* task =
     Async_io.ready_one
       [ (), w1; (), w2 ]
       `Write
       ~f:(fun () fd -> assert (Unix.write fd (Bytes.of_string "0") 0 1 = 1))
   in
   Async_io.Task.await task
   >>= function
   | Error _ -> assert false
   | Ok () ->
     Unix.close r1;
     Unix.close r2;
     let* () = Async_io.close w1 in
     let+ () = Async_io.close w2 in
     print_endline "successful write");
  [%expect {| successful write |}]
;;

let%expect_test "cancel task" =
  (Scheduler.Run.go config ~on_event:(fun _ _ -> ())
   @@ fun () ->
   let r, w = Unix.pipe ~cloexec:true () in
   if not Sys.win32 then Unix.set_nonblock r;
   let* task = Async_io.ready r `Read ~f:ignore in
   Fiber.fork_and_join_unit
     (fun () ->
        Async_io.Task.await task
        >>= function
        | Ok () | Error (`Exn _) -> assert false
        | Error `Cancelled ->
          Unix.close w;
          let+ () = Async_io.close r in
          print_endline "successfully cancelled")
     (fun () -> Async_io.Task.cancel task));
  [%expect {| successfully cancelled |}]
;;

let%expect_test "SIGCHLD wakeups survive interrupted high-concurrency builds" =
  let rec loop n =
    if n = 0
    then print_endline "passed 20 fresh interrupted runs"
    else if run_signal_cleanup_repro ()
    then loop (n - 1)
  in
  loop 20;
  [%expect {| passed 20 fresh interrupted runs |}]
;;
