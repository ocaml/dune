open Stdune
include Dune_scheduler
open Dune_tests_common

let () = init ()

let default =
  Dune_engine.Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = Time.Span.of_secs 0.3) ?(config = default) f =
  try Scheduler.Run.go ~timeout config ~file_watcher:No_watcher f with
  | Shutdown.E Requested -> ()
;;

let%expect_test "raise inside Scheduler.Run.go" =
  (try
     (go
      @@ fun () ->
      Fiber.fork_and_join_unit
        (fun () ->
           print_endline "t1";
           Fiber.return ())
        (fun () -> raise Exit));
     assert false
   with
   | Dune_util.Report_error.Already_reported -> print_endline "--> exception observed");
  [%expect
    {|
    t1
    Error: exception Stdlib.Exit

    I must not crash.  Uncertainty is the mind-killer. Exceptions are the
    little-death that brings total obliteration.  I will fully express my cases.
    Execution will pass over me and through me.  And when it has gone past, I
    will unwind the stack along its path.  Where the cases are handled there will
    be nothing.  Only I will remain.
    --> exception observed |}]
;;

let canonical_signal_number signal =
  let signal = Signal.to_int signal in
  let previous_mask = Unix.sigprocmask SIG_UNBLOCK [ signal ] in
  Exn.protect
    ~finally:(fun () -> ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      let mask_without_signal = Unix.sigprocmask SIG_BLOCK [] in
      ignore (Unix.sigprocmask SIG_BLOCK [ signal ] : int list);
      let mask_with_signal = Unix.sigprocmask SIG_BLOCK [] in
      match
        List.filter mask_with_signal ~f:(fun signal ->
          not (List.mem mask_without_signal signal ~equal:Int.equal))
      with
      | [ signal ] -> signal
      | mask ->
        Code_error.raise
          "could not determine the signal's mask representation"
          [ "mask", Dyn.list Dyn.int mask ])
;;

let terminal_signals_are_blocked terminal_signals =
  let mask = Unix.sigprocmask SIG_BLOCK [] in
  List.for_all terminal_signals ~f:(fun signal -> List.mem mask signal ~equal:Int.equal)
;;

let%expect_test "threaded console handles terminal signals in the console thread" =
  let terminal_signals = List.map Terminal_signals.signals ~f:Signal.to_int in
  let terminal_signals_in_mask =
    List.map Terminal_signals.signals ~f:canonical_signal_number
  in
  let console_thread_blocked = ref None in
  let observation_mutex = Mutex.create () in
  let observed = Condition.create () in
  let observe_from_console_thread () =
    Mutex.protect observation_mutex (fun () ->
      if Option.is_none !console_thread_blocked
      then (
        console_thread_blocked
        := Some (terminal_signals_are_blocked terminal_signals_in_mask);
        Condition.broadcast observed))
  in
  let wait_for_console_thread_observation () =
    Mutex.protect observation_mutex (fun () ->
      while Option.is_none !console_thread_blocked do
        Condition.wait observed observation_mutex
      done;
      Option.value_exn !console_thread_blocked)
  in
  let previous_mask = Unix.sigprocmask SIG_BLOCK [] in
  Exn.protect
    ~finally:(fun () ->
      Console.Backend.set Console.Backend.dumb;
      ignore (Unix.sigprocmask SIG_SETMASK previous_mask : int list))
    ~f:(fun () ->
      ignore (Unix.sigprocmask SIG_UNBLOCK terminal_signals : int list);
      Printf.printf
        "main before start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      let module Test_console = struct
        let start () = ()
        let render (_ : Dune_threaded_console.state) = ()

        let handle_user_events ~now ~time_budget:_ (_ : Mutex.t) _ =
          observe_from_console_thread ();
          Unix.sleepf 0.01;
          now
        ;;

        let reset () = ()
        let reset_flush_history () = ()
        let finish () = ()
      end
      in
      Console.Backend.set
        (Dune_threaded_console.make ~frames_per_second:60 (module Test_console));
      Printf.printf
        "main after start blocked: %b\n"
        (terminal_signals_are_blocked terminal_signals_in_mask);
      Printf.printf
        "console thread blocked: %b\n"
        (wait_for_console_thread_observation ()));
  [%expect
    {|
    main before start blocked: false
    main after start blocked: true
    console thread blocked: false
    |}]
;;
