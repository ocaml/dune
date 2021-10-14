open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

let%expect_test _ =
  let mutex = Mutex.create () in
  let critical_section ~f =
    Mutex.lock mutex;
    Exn.protect ~f ~finally:(fun () -> Mutex.unlock mutex)
  in
  let events_buffer = ref [] in
  let watcher =
    Dune_file_watcher.create_default
      ~scheduler:
        { spawn_thread = (fun f -> ignore (Thread.create f () : Thread.t))
        ; thread_safe_send_emit_events_job =
            (fun job ->
              Mutex.lock mutex;
              let events = job () in
              events_buffer := !events_buffer @ events;
              Mutex.unlock mutex)
        }
  in
  let try_to_get_events () =
    critical_section ~f:(fun () ->
        match !events_buffer with
        | [] -> None
        | list ->
          events_buffer := [];
          Some
            (List.map list ~f:(function
              | Dune_file_watcher.Event.Sync -> assert false
              | Queue_overflow -> assert false
              | Fs_memo_event e -> e
              | Watcher_terminated -> assert false)))
  in
  let print_events n = print_events ~try_to_get_events ~expected:n in
  Dune_file_watcher.wait_for_initial_watches_established_blocking watcher;
  Stdio.Out_channel.write_all "x" ~data:"x";
  print_events 1;
  [%expect {|
    { path = In_source_tree "."; kind = "Created" }
    { path = In_source_tree "x"; kind = "Unknown" }
    Got more events than expected: expected 1, saw 2 |}];
  Unix.rename "x" "y";
  print_events 0;
  [%expect {|
|}];
  Dune_file_watcher.For_tests.suspend watcher;
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  Dune_file_watcher.For_tests.resume watcher;
  print_events 0;
  [%expect.unreachable];
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  print_events 1;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "pid unavailable")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune_file_watcher.For_tests.suspend in file "src/dune_file_watcher/dune_file_watcher.ml", line 518, characters 40-47
  Called from Dune_file_watcher_tests_macos.(fun) in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml", line 47, characters 2-45
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
