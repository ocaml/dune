open Stdune

let temp_dir () = Temp.create Dir ~prefix:"copyfile" ~suffix:"test"

let%expect_test "copy file simple" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file src "foobarbaz";
  Io.copy_file ~src ~dst ();
  print_endline (Io.read_file dst);
  [%expect {| foobarbaz |}]

let%expect_test "copy file overwrite" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file src "foobarbaz";
  Io.write_file dst "xxx";
  Io.copy_file ~src ~dst ();
  print_endline (Io.read_file dst);
  [%expect {| foobarbaz |}]

let%expect_test "copy file chmod" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file src "foobarbaz";
  Io.copy_file ~chmod:(fun _ -> 428) ~src ~dst ();
  print_endline (Io.read_file dst);
  Printf.printf "permissions: %d\n" (Path.stat_exn dst).st_perm;
  [%expect {|
    foobarbaz
    permissions: 428 |}]

let%expect_test "copy file on a directory" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Unix.mkdir (Path.to_string src) 0o755;
  Io.copy_file ~src ~dst ();
  let dst = Path.to_string dst in
  Format.printf "exists: %B; is directory: %B @." (Sys.file_exists dst)
    (Sys.is_directory dst);
  [%expect {| exists: true; is directory: true |}]
