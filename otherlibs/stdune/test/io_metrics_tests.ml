open Stdune

let ok_exn = function
  | Ok value -> value
  | Error error -> Unix_error.Detailed.raise error
;;

let timer_changed timer before =
  match Time.Span.compare (Counter.Timer.read timer) before with
  | Gt -> true
  | Eq | Lt -> false
;;

let%expect_test "IO metrics are attributed to the operation being measured" =
  let dir = Temp.create Dir ~prefix:"io-metrics" ~suffix:"test" in
  let file = Path.relative dir "file" in
  let file_read_before = Counter.Timer.read Metrics.File_read.time in
  let file_write_count_before = Counter.read Metrics.File_write.count in
  let file_write_before = Counter.Timer.read Metrics.File_write.time in
  let directory_read_before = Counter.Timer.read Metrics.Directory_read.time in
  Io.write_file file "contents";
  Dune_tests_common.print_dyn
    (Dyn.record
       [ ( "file_write_count"
         , Dyn.int (Counter.read Metrics.File_write.count - file_write_count_before) )
       ; ( "file_write_time_changed"
         , Dyn.bool (timer_changed Metrics.File_write.time file_write_before) )
       ; ( "file_read_time_changed"
         , Dyn.bool (timer_changed Metrics.File_read.time file_read_before) )
       ; ( "directory_read_time_changed"
         , Dyn.bool (timer_changed Metrics.Directory_read.time directory_read_before) )
       ]);
  [%expect
    {|
    { file_write_count = 1
    ; file_write_time_changed = true
    ; file_read_time_changed = false
    ; directory_read_time_changed = false
    }
    |}];
  let file_read_before = Counter.Timer.read Metrics.File_read.time in
  let file_write_count_before = Counter.read Metrics.File_write.count in
  let file_write_before = Counter.Timer.read Metrics.File_write.time in
  let directory_read_before = Counter.Timer.read Metrics.Directory_read.time in
  Io.write_lines file [ "one"; "two" ];
  Dune_tests_common.print_dyn
    (Dyn.record
       [ ( "file_write_count"
         , Dyn.int (Counter.read Metrics.File_write.count - file_write_count_before) )
       ; ( "file_write_time_changed"
         , Dyn.bool (timer_changed Metrics.File_write.time file_write_before) )
       ; ( "file_read_time_changed"
         , Dyn.bool (timer_changed Metrics.File_read.time file_read_before) )
       ; ( "directory_read_time_changed"
         , Dyn.bool (timer_changed Metrics.Directory_read.time directory_read_before) )
       ]);
  [%expect
    {|
    { file_write_count = 1
    ; file_write_time_changed = true
    ; file_read_time_changed = false
    ; directory_read_time_changed = false
    }
    |}]
;;

let%expect_test "directory metrics count directory scans" =
  let dir = Temp.create Dir ~prefix:"directory-metrics" ~suffix:"test" in
  let first = Path.relative dir "first" in
  let second = Path.relative dir "second" in
  Path.mkdir_p first;
  Path.mkdir_p second;
  let count_before = Counter.read Metrics.Directory_read.count in
  let time_before = Counter.Timer.read Metrics.Directory_read.time in
  ignore (Readdir.read_directory (Path.to_string first) |> ok_exn : string list);
  ignore
    (Readdir.read_directory_with_kinds (Path.to_string second) |> ok_exn
     : (string * Unix.file_kind) list);
  Dune_tests_common.print_dyn
    (Dyn.record
       [ "count", Dyn.int (Counter.read Metrics.Directory_read.count - count_before)
       ; "time_changed", Dyn.bool (timer_changed Metrics.Directory_read.time time_before)
       ]);
  [%expect
    {|
    { count = 2; time_changed = true }
    |}]
;;
