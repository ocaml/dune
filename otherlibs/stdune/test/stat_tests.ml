open Stdune
open Dune_tests_common

let () = init ()
let stat path = Stat.stat (Path.to_string path)
let print_bool name value = Printf.printf "%s: %b\n" name value
let ordering_of_float x y = Ordering.of_int (Stdlib.Float.compare x y)

let%expect_test "Stat.stat matches Unix.stat on stable fields" =
  let dir = Temp.create Dir ~prefix:"stat" ~suffix:"test" in
  let path = Path.relative dir "file" in
  Io.write_file path "hello";
  let stat = stat path in
  let unix = Unix.stat (Path.to_string path) in
  print_bool "size_matches" (Int.equal stat.size unix.st_size);
  print_bool "perm_matches" (Int.equal stat.perm unix.st_perm);
  print_bool "kind_matches" (File_kind.equal stat.kind unix.st_kind);
  print_bool "mtime_is_positive" (Time.compare stat.mtime (Time.of_ns 0) = Gt);
  [%expect
    {|
    size_matches: true
    perm_matches: true
    kind_matches: true
    mtime_is_positive: true
  |}]
;;

let%expect_test "Stat.stat observes mtime updates" =
  let dir = Temp.create Dir ~prefix:"stat" ~suffix:"test" in
  let path = Path.relative dir "file" in
  Io.write_file path "hello";
  let now = Unix.gettimeofday () +. 5.0 in
  let later = now +. 2.0 in
  Unix.utimes (Path.to_string path) now now;
  let first = stat path in
  Unix.utimes (Path.to_string path) later later;
  let second = stat path in
  print_bool "mtime_changed" (Time.compare first.mtime second.mtime <> Eq);
  print_bool "mtime_increases" (Time.compare first.mtime second.mtime = Lt);
  [%expect
    {|
    mtime_changed: true
    mtime_increases: true
  |}]
;;

let%expect_test "Stat.stat keeps same-second directory mtime changes seen by Unix.stat" =
  let dir = Temp.create Dir ~prefix:"stat" ~suffix:"test" in
  let path = Path.to_string dir in
  let base = Stdlib.Float.floor (Unix.gettimeofday ()) +. 5.0 in
  let first_mtime = base +. 0.25 in
  let second_mtime = base +. 0.75 in
  Unix.utimes path first_mtime first_mtime;
  let first_unix = Unix.stat path in
  let first = Stat.stat path in
  Unix.utimes path second_mtime second_mtime;
  let second_unix = Unix.stat path in
  let second = Stat.stat path in
  let unix_order = ordering_of_float first_unix.st_mtime second_unix.st_mtime in
  let stat_order = Time.compare first.mtime second.mtime in
  let unix_changed = not (Ordering.is_eq unix_order) in
  print_bool
    "does_not_drop_unix_mtime_changes"
    ((not unix_changed) || not (Ordering.is_eq stat_order));
  print_bool
    "preserves_unix_mtime_ordering"
    ((not unix_changed)
     || Int.equal (Ordering.to_int unix_order) (Ordering.to_int stat_order));
  [%expect
    {|
    does_not_drop_unix_mtime_changes: true
    preserves_unix_mtime_ordering: true
  |}]
;;
