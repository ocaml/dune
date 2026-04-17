open Lev

let flags = Loop.Flag.(Set.singleton Nosigmask)

let%expect_test "signal handling" =
  let signal = Sys.sigusr1 in
  let loop = Loop.create ~flags () in
  let signal_watcher =
    Signal.create ~signal (fun t ->
      print_endline "received signal";
      ignore (Unix.sigprocmask SIG_BLOCK [ signal ]);
      Signal.stop t loop)
  in
  let idle =
    Idle.create (fun idle ->
      print_endline "sending signal";
      Unix.kill (Unix.getpid ()) signal;
      Idle.stop idle loop)
  in
  Signal.start signal_watcher loop;
  Idle.start idle loop;
  Loop.run_until_done loop;
  [%expect
    {|
    sending signal
    received signal |}]
;;

let%expect_test "manual signal feeding" =
  let signal = Sys.sigusr2 in
  ignore (Unix.sigprocmask SIG_BLOCK [ signal ]);
  let thread =
    Thread.create
      (fun () ->
         print_endline "thread: awaiting signal";
         ignore (Thread.wait_signal [ signal ]);
         Loop.feed_signal ~signal;
         print_endline "thread: awaited signal")
      ()
  in
  let loop = Loop.create () in
  let signal_watcher =
    Signal.create ~signal (fun w ->
      print_endline "lev: received signal";
      Signal.stop w loop)
  in
  Signal.start signal_watcher loop;
  Unix.kill (Unix.getpid ()) signal;
  Loop.run_until_done loop;
  Thread.join thread;
  [%expect
    {|
    thread: awaiting signal
    thread: awaited signal
    lev: received signal |}]
;;
