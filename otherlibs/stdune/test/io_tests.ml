open Stdune

let temp_dir ~prefix = Temp.create Dir ~prefix ~suffix:"test"

let%expect_test "read_file and write_file return results" =
  let dir = temp_dir ~prefix:"io" in
  List.iter [ true; false ] ~f:(fun binary ->
    Printf.printf "binary: %b\n" binary;
    let file = Path.relative dir (Bool.to_string binary) in
    let filename = Path.to_string file in
    let contents = "hello\n\000world\n" in
    let perm = Permissions.Mode.of_int 0o600 in
    Io.write_file ~binary ~perm file contents |> Result.ok_exn;
    let read = Io.String_path.read_file ~binary filename |> Result.ok_exn in
    Printf.printf "round trip: %b\n" (String.equal contents read);
    Printf.printf
      "permissions respected: %b\n"
      (Sys.win32 || Int.equal (Unix.stat filename).st_perm 0o600);
    Io.String_path.write_file ~binary ~perm filename "" |> Result.ok_exn;
    Printf.printf "truncated: %S\n" (Io.read_file ~binary file |> Result.ok_exn));
  [%expect
    {|
    binary: true
    round trip: true
    permissions respected: true
    truncated: ""
    binary: false
    round trip: true
    permissions respected: true
    truncated: ""
    |}]
;;

let%expect_test "read_file and write_file return IO errors" =
  let dir = temp_dir ~prefix:"io" in
  let missing = Path.relative dir "missing/file" in
  let print_error = function
    | Ok _ -> print_endline "unexpected success"
    | Error (Unix.Unix_error _) -> print_endline "Unix_error"
    | Error (Sys_error _) -> print_endline "Sys_error"
    | Error exn -> print_endline (Printexc.to_string exn)
  in
  List.iter [ true; false ] ~f:(fun binary ->
    Printf.printf "binary: %b\n" binary;
    Io.read_file ~binary missing |> print_error;
    Io.write_file ~binary missing "contents" |> print_error;
    Io.String_path.read_file ~binary (Path.to_string dir) |> print_error;
    Io.String_path.write_file ~binary (Path.to_string dir) "contents" |> print_error;
    (match Io.read_file_exn ~binary missing with
     | _ -> print_endline "unexpected success"
     | exception exn -> print_error (Error exn));
    match Io.write_file_exn ~binary missing "contents" with
    | () -> print_endline "unexpected success"
    | exception exn -> print_error (Error exn));
  [%expect
    {|
    binary: true
    Unix_error
    Unix_error
    Unix_error
    Unix_error
    Unix_error
    Unix_error
    binary: false
    Sys_error
    Sys_error
    Sys_error
    Sys_error
    Sys_error
    Sys_error
    |}]
;;

let%expect_test "write_fd returns errors without closing the descriptor" =
  let dir = temp_dir ~prefix:"io-fd" in
  let file = Path.relative dir "file" in
  Io.write_file_exn file "contents";
  Io.with_file_in file ~f:(fun ic ->
    let fd = Unix.descr_of_in_channel ic |> Fd.unsafe_of_unix_file_descr in
    (match Io.write_fd fd "new contents" with
     | Error (Unix.Unix_error (EBADF, _, _)) -> print_endline "write error returned"
     | Error exn -> raise exn
     | Ok () -> print_endline "unexpected success");
    print_endline (input_line ic));
  [%expect
    {|
    write error returned
    contents
    |}]
;;

let%expect_test "portable symlink through symlinked dst dir" =
  (let root = temp_dir ~prefix:"symlink" in
   let src = Path.relative root "src/file" in
   Path.mkdir_p (Path.parent_exn src);
   Io.write_file_exn src "contents";
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
   | Ok "contents" -> print_endline "target reachable"
   | Ok contents -> Printf.printf "unexpected contents: %s\n" contents
   | Error (Sys_error _ | Unix.Unix_error _) -> print_endline "target broken"
   | Error exn -> raise exn);
  [%expect {| target reachable |}]
;;

let temp_dir () = temp_dir ~prefix:"copyfile"

let%expect_test "copy file simple" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  Io.copy_file ~src ~dst ();
  print_endline (Io.read_file_exn dst);
  [%expect {| foobarbaz |}]
;;

let%expect_test "copy file overwrite" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  Io.write_file_exn dst "xxx";
  Io.copy_file ~src ~dst ();
  print_endline (Io.read_file_exn dst);
  [%expect {| foobarbaz |}]
;;

let%expect_test "copy file chmod" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  Io.copy_file
    ~chmod:(fun _ ->
      let open Permissions in
      Mode.create ~user:(read + write) ~group:(read + execute) ~other:read ())
    ~src
    ~dst
    ();
  print_endline (Io.read_file_exn dst);
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
  Io.write_file_exn src "foobarbaz";
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
