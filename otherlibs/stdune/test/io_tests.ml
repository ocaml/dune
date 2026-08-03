open Stdune

let temp_dir ~prefix = Temp.create Dir ~prefix ~suffix:"test"

let%expect_test "portable symlink through symlinked dst dir" =
  (let root = temp_dir ~prefix:"symlink" in
   let src = Path.relative root "src/file" in
   Path.mkdir_p (Path.parent_exn src);
   Io.write_file src "contents";
   let dst =
     let real = Path.relative root "real/deeper" in
     let () =
       let dst_parent = Path.relative real "self-in-path" in
       Path.mkdir_p dst_parent
     in
     let link = Path.relative root "link" in
     Unix.symlink (Path.to_string real) (Path.to_string link);
     Path.relative link "self-in-path/dune"
   in
   Io.portable_symlink ~src ~dst;
   match Io.read_file dst with
   | "contents" -> print_endline "target reachable"
   | contents -> Printf.printf "unexpected contents: %s\n" contents
   | (exception Sys_error _) | (exception Unix.Unix_error _) ->
     print_endline "target broken");
  [%expect {| target reachable |}]
;;

let temp_dir () = temp_dir ~prefix:"copyfile"

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
  Io.copy_file
    ~chmod:(fun _ ->
      let open Permissions in
      Mode.create ~user:(read + write) ~group:(read + execute) ~other:read ())
    ~src
    ~dst
    ();
  print_endline (Io.read_file dst);
  Printf.printf "permissions: %d\n" (Unix.stat (Path.to_string dst)).st_perm;
  [%expect
    {|
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
