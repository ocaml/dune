open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

(* Test that the file watcher detects file system changes.
   We only check that the relevant paths are eventually seen, not exact
   event counts or kinds, since FSEvents on macOS can coalesce events. *)
let%expect_test _ =
  let mutex = Mutex.create () in
  let events_buffer = ref [] in
  let watcher =
    Dune_file_watcher.create_default
      ~fsevents_debounce:(Time.Span.of_secs 0.)
      ~scheduler:
        { spawn_thread = (fun f -> Thread.create f ())
        ; thread_safe_send_emit_events_job =
            (fun job ->
              critical_section mutex ~f:(fun () ->
                let events = job () in
                events_buffer := !events_buffer @ events))
        }
      ~watch_exclusions:[]
      ()
  in
  let try_to_get_events () =
    critical_section mutex ~f:(fun () ->
      match !events_buffer with
      | [] -> None
      | list ->
        events_buffer := [];
        Some
          (List.filter_map list ~f:(function
             | Dune_file_watcher.Event.Sync _ -> None
             | Queue_overflow -> assert false
             | Fs_memo_event e -> Some e
             | Watcher_terminated -> assert false)))
  in
  let wait_for paths = wait_for_paths ~try_to_get_events ~paths in
  Dune_file_watcher.wait_for_initial_watches_established_blocking watcher;
  (* Test 1: file creation is detected *)
  Stdio.Out_channel.write_all "x" ~data:"x";
  wait_for [ "x" ];
  [%expect {| saw: x |}];
  (* Test 2: file rename is detected (both old and new paths) *)
  Unix.rename "x" "y";
  wait_for [ "x"; "y" ];
  [%expect {| saw: x, y |}];
  (* Test 3: directory and file creation is detected *)
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  wait_for [ "d/w/x" ];
  [%expect {| saw: d/w/x |}];
  (* Test 4: file creation in existing directory is detected *)
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  wait_for [ "d/w/y" ];
  [%expect {| saw: d/w/y |}]
;;
