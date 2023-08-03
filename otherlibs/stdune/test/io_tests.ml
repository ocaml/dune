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
;;

let%expect_test "copy file overwrite" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file src "foobarbaz";
  Io.write_file dst "xxx";
  Io.copy_file ~src ~dst ();
  print_endline (Io.read_file dst);
  [%expect {| foobarbaz |}]
;;

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
;;

let%expect_test "copy file - no src" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  match Io.copy_file ~src ~dst () with
  | () -> assert false
  | exception Sys_error s ->
    let s =
      let _, exn = String.lsplit2_exn s ~on:':' in
      sprintf "$PATH:%s" exn
    in
    print_endline s;
    [%expect {| $PATH: No such file or directory |}]
;;

let%expect_test "copy file - src is a directory" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Unix.mkdir (Path.to_string src) 0o755;
  Io.copy_file ~src ~dst ();
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Sys_error "Is a directory") |}]
;;

let%expect_test "copy file - dst is a directory" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file src "foobarbaz";
  Unix.mkdir (Path.to_string dst) 0o755;
  Unix.sleepf 0.5;
  match Io.copy_file ~src ~dst () with
  | _ -> assert false
  | exception Sys_error s ->
    let s =
      let _, exn = String.lsplit2_exn s ~on:':' in
      sprintf "$DIR:%s" exn
    in
    print_endline s;
    [%expect {| $DIR: Is a directory |}]
;;

let%expect_test "making a directory for an existing file" =
  let dir = temp_dir () in
  let fn = Path.relative dir "foo" in
  Io.write_file fn "";
  (* This does not error, but it will if it ends with a "/" on MacOS *)
  ignore (Fpath.mkdir (Path.to_string fn));
  [%expect {| |}];
  Path.mkdir_p fn;
  (* This in turn does not error *)
  [%expect {| |}]
;;
