open Stdune
include Dune_scheduler
open Dune_tests_common

let () = init ()

let default =
  Clflags.display := Short;
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

let%expect_test "the last DAP request waits to reacquire its slot" =
  let open Fiber.O in
  go (fun () ->
    let slot = Scheduler.Job_slot.create ~job_slots:1 in
    Scheduler.Job_slot.run slot (Fiber.Cancel.create ()) (fun () ->
      let occupied = Fiber.Ivar.create () in
      let finishing = Fiber.Ivar.create () in
      let finished = ref false in
      let+ () =
        Fiber.fork_and_join_unit
          (fun () ->
             Scheduler.with_job_slot ~job_slots:1 (fun () ->
               let* () = Fiber.Ivar.fill occupied () in
               let+ () = Fiber.Ivar.read finishing in
               printfn "request finished while slot occupied: %b" !finished))
          (fun () ->
             let+ () =
               Scheduler.Job_slot.with_ slot ~f:(fun () ->
                 let* () = Fiber.Ivar.read occupied in
                 Fiber.Ivar.fill finishing ())
             in
             finished := true)
      in
      printfn "request finished after release: %b" !finished));
  [%expect
    {|
    request finished while slot occupied: false
    request finished after release: true |}]
;;

let%expect_test "a new DAP request interrupts queued slot reacquisition" =
  let open Fiber.O in
  go (fun () ->
    let slot = Scheduler.Job_slot.create ~job_slots:1 in
    let* () =
      Scheduler.Job_slot.run slot (Fiber.Cancel.create ()) (fun () ->
        let occupied = Fiber.Ivar.create () in
        let finishing = Fiber.Ivar.create () in
        let interrupted = Fiber.Ivar.create () in
        let+ () =
          Fiber.all_concurrently_unit
            [ (let* () =
                 Scheduler.Job_slot.with_ slot ~f:(fun () ->
                   let* () = Fiber.Ivar.read occupied in
                   Fiber.Ivar.fill finishing ())
               in
               Fiber.Ivar.fill interrupted ())
            ; Scheduler.with_job_slot ~job_slots:1 (fun () ->
                let* () = Fiber.Ivar.fill occupied () in
                Fiber.Ivar.read interrupted)
            ; (let* () = Fiber.Ivar.read finishing in
               Scheduler.Job_slot.with_ slot ~f:(fun () ->
                 let+ () = Fiber.Ivar.read interrupted in
                 print_endline "reacquisition interrupted"))
            ]
        in
        print_endline "reacquired")
    in
    Scheduler.with_job_slot ~job_slots:1 (fun () ->
      print_endline "released";
      Fiber.return ()));
  [%expect
    {|
    reacquisition interrupted
    reacquired
    released |}]
;;

let%expect_test "completed DAP requests share queued slot reacquisition" =
  let open Fiber.O in
  go (fun () ->
    let slot = Scheduler.Job_slot.create ~job_slots:1 in
    let* () =
      Scheduler.Job_slot.run slot (Fiber.Cancel.create ()) (fun () ->
        let occupied = Fiber.Ivar.create () in
        let first_finishing = Fiber.Ivar.create () in
        let second_finishing = Fiber.Ivar.create () in
        let finished = ref 0 in
        let+ () =
          Fiber.all_concurrently_unit
            [ (let+ () =
                 Scheduler.Job_slot.with_ slot ~f:(fun () ->
                   let* () = Fiber.Ivar.read occupied in
                   Fiber.Ivar.fill first_finishing ())
               in
               incr finished)
            ; Scheduler.with_job_slot ~job_slots:1 (fun () ->
                let* () = Fiber.Ivar.fill occupied () in
                let+ () = Fiber.Ivar.read second_finishing in
                printfn "requests finished while slot occupied: %d" !finished)
            ; (let* () = Fiber.Ivar.read first_finishing in
               let+ () =
                 Scheduler.Job_slot.with_ slot ~f:(fun () ->
                   Fiber.Ivar.fill second_finishing ())
               in
               incr finished)
            ]
        in
        printfn "requests finished after release: %d" !finished)
    in
    Scheduler.with_job_slot ~job_slots:1 (fun () ->
      print_endline "released";
      Fiber.return ()));
  [%expect
    {|
    requests finished while slot occupied: 0
    requests finished after release: 2
    released |}]
;;

let%expect_test "ending a DAP action cancels queued slot reacquisition" =
  let open Fiber.O in
  go (fun () ->
    let slot = Scheduler.Job_slot.create ~job_slots:1 in
    let started = Fiber.Ivar.create () in
    let finish = Fiber.Ivar.create () in
    let occupied = Fiber.Ivar.create () in
    let request_finished = Fiber.Ivar.create () in
    let* () =
      Fiber.all_concurrently_unit
        [ Scheduler.Job_slot.run slot (Fiber.Cancel.create ()) (fun () ->
            let* () = Fiber.Ivar.fill started () in
            Fiber.Ivar.read finish)
        ; (let* () = Fiber.Ivar.read started in
           let* () =
             Scheduler.Job_slot.with_ slot ~f:(fun () ->
               let* () = Fiber.Ivar.read occupied in
               Fiber.Ivar.fill finish ())
           in
           let+ () = Fiber.Ivar.fill request_finished () in
           print_endline "request finished")
        ; Scheduler.with_job_slot ~job_slots:1 (fun () ->
            let* () = Fiber.Ivar.fill occupied () in
            Fiber.Ivar.read request_finished)
        ]
    in
    Scheduler.with_job_slot ~job_slots:1 (fun () ->
      print_endline "released";
      Fiber.return ()));
  [%expect
    {|
    request finished
    released |}]
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

let%expect_test "threaded console snapshots, status replacement and history resets" =
  let input, wakeup = Unix.pipe ~cloexec:true () in
  let observation_mutex = Mutex.create () in
  let observed = Condition.create () in
  let frames = Queue.create () in
  let events = ref 0 in
  let resets = ref 0
  and history_resets = ref 0 in
  let final_render = ref false
  and finishes = ref 0 in
  let module Recording_console = struct
    let start () = ()

    let render (state : Dune_threaded_console.state) =
      Mutex.protect observation_mutex (fun () ->
        if state.finish_requested
        then final_render := true
        else
          Queue.push
            frames
            ( List.map (Queue.to_list state.messages) ~f:(fun message ->
                User_message.to_string message |> String.trim)
            , Option.map state.status_line ~f:(Format.asprintf "%a" Pp.to_fmt) );
        Condition.broadcast observed)
    ;;

    let handle_user_events ~now:_ ~time_budget:_ (_ : Mutex.t) _ =
      Mutex.protect observation_mutex (fun () ->
        incr events;
        Condition.broadcast observed);
      let byte = Bytes.create 1 in
      ignore (Unix.read input byte 0 1 : int);
      Time.now ()
    ;;

    let reset () = incr resets
    let reset_flush_history () = incr history_resets
    let finish () = incr finishes
  end
  in
  let module Backend =
    (val Dune_threaded_console.make ~frames_per_second:60 (module Recording_console))
  in
  let wait_for_frame count =
    Mutex.protect observation_mutex (fun () ->
      while Queue.length frames < count do
        Condition.wait observed observation_mutex
      done)
  in
  let wake () = ignore (Unix.write wakeup (Bytes.of_string "x") 0 1 : int) in
  Backend.start ();
  Exn.protect
    ~finally:(fun () ->
      Unix.close wakeup;
      Backend.finish ();
      Unix.close input)
    ~f:(fun () ->
      wait_for_frame 1;
      Backend.print_user_message (User_message.make [ Pp.verbatim "one" ]);
      Backend.print_user_message (User_message.make [ Pp.verbatim "two" ]);
      Backend.set_status_line (Some (Pp.verbatim "busy"));
      wake ();
      wait_for_frame 2;
      Backend.print_if_no_status_line (Pp.verbatim "not displayed");
      wake ();
      Mutex.protect observation_mutex (fun () ->
        while !events < 3 do
          Condition.wait observed observation_mutex
        done;
        printfn "no-op preserves frame count: %d" (Queue.length frames));
      Backend.reset ();
      wake ();
      wait_for_frame 3;
      Backend.reset_flush_history ();
      Backend.print_user_message (User_message.make [ Pp.verbatim "after reset" ]);
      wake ();
      wait_for_frame 4);
  List.iter (Queue.to_list frames) ~f:(fun (messages, status) ->
    Dyn.to_string
      (Dyn.pair (Dyn.list Dyn.string) (Dyn.option Dyn.string) (messages, status))
    |> print_endline);
  printfn "reset calls: %d; history reset calls: %d" !resets !history_resets;
  printfn "final render: %b; finish calls: %d" !final_render !finishes;
  [%expect
    {|
    no-op preserves frame count: 2
    ([], None)
    ([ "one"; "two" ], Some "busy")
    ([], None)
    ([ "after reset" ], None)
    reset calls: 1; history reset calls: 1
    final render: true; finish calls: 1 |}]
;;

type console_failure =
  | Fail_render
  | Fail_events
  | Fail_events_locked
  | Fail_finish

let console_failure failure =
  let observation_mutex = Mutex.create () in
  let observed = Condition.create () in
  let entered = ref false in
  let finishes = ref 0 in
  let zero_budget = ref false in
  let enter () =
    Mutex.protect observation_mutex (fun () ->
      entered := true;
      Condition.broadcast observed)
  in
  let module Failing_console = struct
    let start () = ()

    let render (_ : Dune_threaded_console.state) =
      match failure with
      | Fail_render ->
        enter ();
        failwith "intentional console render failure"
      | _ -> ()
    ;;

    let handle_user_events ~now:_ ~time_budget mutex _ =
      zero_budget := Poly.equal time_budget Time.Span.zero;
      match failure with
      | Fail_events ->
        enter ();
        failwith "intentional console event failure"
      | Fail_events_locked ->
        Mutex.lock mutex;
        enter ();
        failwith "intentional locked console event failure"
      | Fail_render | Fail_finish -> Time.now ()
    ;;

    let reset () = ()
    let reset_flush_history () = ()

    let finish () =
      incr finishes;
      match failure with
      | Fail_finish -> failwith "intentional console cleanup failure"
      | _ -> ()
    ;;
  end
  in
  let module Backend =
    (val Dune_threaded_console.make ~frames_per_second:max_int (module Failing_console))
  in
  Backend.start ();
  (match failure with
   | Fail_finish -> ()
   | Fail_render | Fail_events | Fail_events_locked ->
     Mutex.protect observation_mutex (fun () ->
       while not !entered do
         Condition.wait observed observation_mutex
       done));
  Backend.finish ();
  printfn "finish returned; cleanup calls: %d" !finishes;
  match failure with
  | Fail_events | Fail_events_locked -> printfn "zero frame budget: %b" !zero_budget
  | Fail_render | Fail_finish -> ()
;;

let%expect_test "threaded console finalizes after a rendering exception" =
  console_failure Fail_render;
  [%expect {| finish returned; cleanup calls: 1 |}]
;;

let%expect_test "threaded console reacquires the mutex after event failures" =
  console_failure Fail_events;
  console_failure Fail_events_locked;
  [%expect
    {|
    finish returned; cleanup calls: 1
    zero frame budget: true
    finish returned; cleanup calls: 1
    zero frame budget: true |}]
;;

let%expect_test "threaded console survives an exception during cleanup" =
  console_failure Fail_finish;
  [%expect {| finish returned; cleanup calls: 1 |}]
;;

let%expect_test "threaded progress flushes queued output before finishing" =
  let input, output = Unix.pipe ~cloexec:true () in
  let saved_stderr = Unix.dup Unix.stderr in
  let channel = Unix.in_channel_of_descr input in
  let module Backend = (val Dune_threaded_console.progress ~frames_per_second:60) in
  Unix.dup2 output Unix.stderr;
  Unix.close output;
  Backend.start ();
  let first, second =
    Exn.protect
      ~finally:(fun () ->
        Backend.finish ();
        Unix.dup2 saved_stderr Unix.stderr;
        Unix.close saved_stderr;
        close_in channel)
      ~f:(fun () ->
        Backend.print_user_message (User_message.make [ Pp.verbatim "first frame" ]);
        let first = input_line channel in
        Backend.print_user_message (User_message.make [ Pp.verbatim "second frame" ]);
        let second = input_line channel in
        Backend.print_if_no_status_line (Pp.verbatim "not displayed");
        first, second)
  in
  print_endline first;
  print_endline second;
  print_endline "finished";
  [%expect
    {|
    first frame
    second frame
    finished |}]
;;
