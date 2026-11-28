open Stdune
open Dune_file_watcher_tests_lib

let%expect_test _ = init ()

let%expect_test "file operations" =
  let watcher, try_to_get_events =
    create_watcher ~fsevents_debounce:(Time.Span.of_secs 0.) ~watch_exclusions:[] ()
  in
  let print_events expected = print_unique_events ~try_to_get_events ~expected in
  let create_file path =
    Unix.close (Unix.openfile path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600)
  in
  create_file "x";
  print_events 1;
  [%expect {| { path = In_source_tree "x"; kind = Created } |}];
  Unix.rename "x" "y";
  print_events 2;
  [%expect
    {|
    { path = In_source_tree "x"; kind = Unknown }
    { path = In_source_tree "y"; kind = Unknown }
    |}];
  let (_ : _) = Fpath.mkdir_p "d/w" in
  print_events 2;
  [%expect
    {|
    { path = In_source_tree "d"; kind = Created }
    { path = In_source_tree "d/w"; kind = Created }
    |}];
  create_file "d/w/x";
  print_events 1;
  [%expect {| { path = In_source_tree "d/w/x"; kind = Created } |}];
  create_file "d/w/y";
  print_events 1;
  [%expect {| { path = In_source_tree "d/w/y"; kind = Created } |}];
  Dune_scheduler.File_watcher.shutdown watcher
;;
