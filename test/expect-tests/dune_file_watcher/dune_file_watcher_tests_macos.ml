open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

let%expect_test _ =
  let (_ : Dune_scheduler.File_watcher.t), try_to_get_events =
    create_watcher ~fsevents_debounce:(Time.Span.of_secs 0.) ~watch_exclusions:[] ()
  in
  let print_events n = print_events ~try_to_get_events ~expected:n in
  Stdio.Out_channel.write_all "x" ~data:"x";
  print_events 2;
  [%expect
    {|
    { path = In_source_tree "."; kind = "Created" }
    { path = In_source_tree "x"; kind = "Unknown" } |}];
  Unix.rename "x" "y";
  print_events 2;
  [%expect
    {|
    { path = In_source_tree "x"; kind = "Unknown" }
    { path = In_source_tree "y"; kind = "Unknown" } |}];
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  print_events 3;
  [%expect
    {|
    { path = In_source_tree "d"; kind = "Created" }
    { path = In_source_tree "d/w"; kind = "Created" }
    { path = In_source_tree "d/w/x"; kind = "Unknown" } |}];
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  print_events 1;
  [%expect {| { path = In_source_tree "d/w/y"; kind = "Unknown" } |}]
;;
