open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

let%expect_test "paths affected by file operations" =
  let (_ : Dune_scheduler.File_watcher.t), try_to_get_events =
    create_watcher ~fsevents_debounce:(Time.Span.of_secs 0.) ~watch_exclusions:[] ()
  in
  (* FSEvents can split or coalesce these operations. Check the affected paths
     together; action-to-kind conversion is tested in [File_watcher]. *)
  Stdio.Out_channel.write_all "x" ~data:"x";
  Unix.rename "x" "y";
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  print_changed_paths ~try_to_get_events ~expected:6;
  [%expect
    {|
    In_source_tree "d"
    In_source_tree "d/w"
    In_source_tree "d/w/x"
    In_source_tree "d/w/y"
    In_source_tree "x"
    In_source_tree "y"
    |}]
;;
