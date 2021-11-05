open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

let%expect_test _ =
  let mutex = Mutex.create () in
  let events_buffer = ref [] in
  let watcher =
    Dune_file_watcher.create_default ~fsevents_debounce:0.
      ~scheduler:
        { spawn_thread = (fun f -> ignore (Thread.create f () : Thread.t))
        ; thread_safe_send_emit_events_job =
            (fun job ->
              critical_section mutex ~f:(fun () ->
                  let events = job () in
                  events_buffer := !events_buffer @ events))
        }
      ()
  in
  let try_to_get_events () =
    critical_section mutex ~f:(fun () ->
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
  print_events 3;
  [%expect.unreachable];
  Unix.rename "x" "y";
  print_events 2;
  [%expect.unreachable];
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  print_events 3;
  [%expect.unreachable];
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  print_events 1;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml:29:48"
  Raised at Dune_file_watcher_tests_macos.(fun).try_to_get_events.(fun) in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml", line 29, characters 48-60
  Called from Stdlib__list.rev_map.rmap_f in file "list.ml", line 103, characters 22-25
  Called from Stdune__List.map in file "otherlibs/stdune-unstable/list.ml", line 5, characters 19-33
  Called from Dune_file_watcher_tests_macos.(fun).try_to_get_events.(fun) in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml", line 28, characters 12-237
  Called from Stdune__Exn.protectx in file "otherlibs/stdune-unstable/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune-unstable/exn.ml", line 18, characters 4-11
  Called from Dune_file_watcher_tests_lib.get_events.(fun) in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_lib.ml", line 46, characters 10-30
  Called from Dune_file_watcher_tests_lib.retry_loop.loop in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_lib.ml", line 25, characters 10-14
  Called from Dune_file_watcher_tests_lib.get_events in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_lib.ml", line 44, characters 6-291
  Called from Dune_file_watcher_tests_lib.print_events in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_lib.ml", line 67, characters 23-62
  Called from Dune_file_watcher_tests_macos.(fun).print_events in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml" (inlined), line 34, characters 23-66
  Called from Dune_file_watcher_tests_macos.(fun) in file "test/expect-tests/dune_file_watcher/dune_file_watcher_tests_macos.ml", line 37, characters 2-16
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
