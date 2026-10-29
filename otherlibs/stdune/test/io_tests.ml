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

let with_symlink_sandbox f =
  Temp.with_temp_dir
    ~parent_dir:(Path.of_string (Sys.getcwd ()))
    ~prefix:"symlink"
    ~suffix:"test"
    ~f:(fun dir -> f (Result.ok_exn dir))
;;

let%expect_test "portable symlink through symlinked dst dir" =
  with_symlink_sandbox (fun root ->
    let src = Path.relative root "src/file" in
    Path.mkdir_p (Path.parent_exn src);
    Io.write_file_exn src "contents";
    let dst =
      let real = Path.relative root "real/deeper" in
      let dst_parent = Path.relative real "self-in-path" in
      Path.mkdir_p dst_parent;
      let link = Path.relative root "link" in
      Unix.symlink (Path.to_string real) (Path.to_string link);
      Path.relative link "self-in-path/dune"
    in
    Io.portable_symlink ~overwrite:true ~src ~dst;
    print_endline (Io.read_file_exn dst));
  [%expect {| contents |}]
;;

let%expect_test "symlink destination observes changes to the source" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "original";
    Io.portable_symlink ~overwrite:false ~src ~dst;
    print_endline (Io.read_file_exn dst);
    Io.write_file_exn src "updated";
    print_endline (Io.read_file_exn dst));
  [%expect
    {|
    original
    updated
    |}]
;;

let%expect_test "symlink overwrite also allows a missing destination" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "contents";
    Io.portable_symlink ~overwrite:true ~src ~dst;
    let { Unix.st_kind; _ } = Unix.lstat (Path.to_string dst) in
    Printf.printf "symbolic link: %b\n" (st_kind = S_LNK);
    print_endline (Io.read_file_exn dst));
  [%expect
    {|
    symbolic link: true
    contents
    |}]
;;

let%expect_test "exclusive symlink rejects an identical target" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "contents";
    Io.portable_symlink ~overwrite:false ~src ~dst;
    let target = Unix.readlink (Path.to_string dst) in
    (match Io.portable_symlink ~overwrite:false ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (EEXIST, _, _) -> print_endline "EEXIST");
    Printf.printf
      "target unchanged: %b\n"
      (String.equal target (Unix.readlink (Path.to_string dst))));
  [%expect
    {|
    EEXIST
    target unchanged: true
    |}]
;;

let%expect_test "exclusive symlink preserves a different target" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let other = Path.relative dir "other" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "replacement";
    Io.write_file_exn other "original";
    Io.portable_symlink ~overwrite:false ~src:other ~dst;
    (match Io.portable_symlink ~overwrite:false ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (EEXIST, _, _) -> print_endline "EEXIST");
    Printf.printf "destination: %s\n" (Io.read_file_exn dst);
    Printf.printf "source: %s\n" (Io.read_file_exn src));
  [%expect
    {|
    EEXIST
    destination: original
    source: replacement
    |}]
;;

let%expect_test "exclusive symlink rejects a dangling destination" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "contents";
    Unix.symlink "missing" (Path.to_string dst);
    (match Io.portable_symlink ~overwrite:false ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (EEXIST, _, _) -> print_endline "EEXIST");
    print_endline (Unix.readlink (Path.to_string dst)));
  [%expect
    {|
    EEXIST
    missing
    |}]
;;

let%expect_test "overwriting an identical symlink is a no-op" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "contents";
    Io.portable_symlink ~overwrite:false ~src ~dst;
    let saved = Path.relative dir "saved" in
    (* Pin the symlink's inode so unlinking and recreating [dst] cannot look
       like a no-op by recycling the inode number. *)
    Unix.link ~follow:false (Path.to_string dst) (Path.to_string saved);
    let { Unix.st_ino = before; _ } = Unix.lstat (Path.to_string saved) in
    Io.portable_symlink ~overwrite:true ~src ~dst;
    let { Unix.st_ino = after; _ } = Unix.lstat (Path.to_string dst) in
    Printf.printf "same inode: %b\n" (Int.equal before after);
    print_endline (Io.read_file_exn dst));
  [%expect
    {|
    same inode: true
    contents
    |}]
;;

let%expect_test "overwriting a symlink changes the link, not its referents" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let other = Path.relative dir "other" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "replacement";
    Io.write_file_exn other "original";
    Io.portable_symlink ~overwrite:false ~src:other ~dst;
    let target = Unix.readlink (Path.to_string dst) in
    Io.portable_symlink ~overwrite:true ~src ~dst;
    Printf.printf
      "target changed: %b\n"
      (not (String.equal target (Unix.readlink (Path.to_string dst))));
    Printf.printf "destination: %s\n" (Io.read_file_exn dst);
    Printf.printf "original referent: %s\n" (Io.read_file_exn other);
    Printf.printf "new referent: %s\n" (Io.read_file_exn src));
  [%expect
    {|
    target changed: true
    destination: replacement
    original referent: original
    new referent: replacement
    |}]
;;

let%expect_test "overwriting a dangling symlink does not create its old target" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    let missing = Path.relative dir "missing" in
    Io.write_file_exn src "contents";
    Unix.symlink "missing" (Path.to_string dst);
    Io.portable_symlink ~overwrite:true ~src ~dst;
    print_endline (Io.read_file_exn dst);
    Printf.printf "old target created: %b\n" (Fpath.exists (Path.to_string missing)));
  [%expect
    {|
    contents
    old target created: false
    |}]
;;

let%expect_test "symlink overwrite does not replace a regular file" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    Io.write_file_exn src "source";
    Io.write_file_exn dst "destination";
    (match Io.portable_symlink ~overwrite:true ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (EEXIST, _, _) -> print_endline "EEXIST");
    print_endline (Io.read_file_exn dst));
  [%expect
    {|
    EEXIST
    destination
    |}]
;;

let%expect_test "symlink overwrite preserves a directory and its contents" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let dst = Path.relative dir "destination" in
    let marker = Path.relative dst "keep" in
    Io.write_file_exn src "source";
    Path.mkdir_p dst;
    Io.write_file_exn marker "original";
    (match Io.portable_symlink ~overwrite:true ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (EEXIST, _, _) -> print_endline "EEXIST");
    print_endline (Io.read_file_exn marker));
  [%expect
    {|
    EEXIST
    original
    |}]
;;

let%expect_test "symlink creation does not create missing parent directories" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let parent = Path.relative dir "missing" in
    let dst = Path.relative parent "destination" in
    Io.write_file_exn src "source";
    (match Io.portable_symlink ~overwrite:false ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (ENOENT, _, _) -> print_endline "ENOENT");
    Printf.printf "parent created: %b\n" (Fpath.exists (Path.to_string parent)));
  [%expect
    {|
    ENOENT
    parent created: false
    |}]
;;

let%expect_test "symlink overwrite propagates a non-directory parent error" =
  with_symlink_sandbox (fun dir ->
    let src = Path.relative dir "source" in
    let parent = Path.relative dir "parent" in
    let dst = Path.relative parent "destination" in
    Io.write_file_exn src "source";
    Io.write_file_exn parent "original";
    (match Io.portable_symlink ~overwrite:true ~src ~dst with
     | () -> print_endline "unexpected success"
     | exception Unix.Unix_error (ENOTDIR, _, _) -> print_endline "ENOTDIR");
    print_endline (Io.read_file_exn parent));
  [%expect
    {|
    ENOTDIR
    original
    |}]
;;

let temp_dir () = temp_dir ~prefix:"copyfile"

let%expect_test "copy file simple" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  Io.copy_file ~overwrite:true ~src ~dst ();
  print_endline (Io.read_file_exn dst);
  [%expect {| foobarbaz |}]
;;

let%expect_test "copy file overwrite" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  Io.write_file_exn dst "xxx";
  Io.copy_file ~overwrite:true ~src ~dst ();
  print_endline (Io.read_file_exn dst);
  [%expect {| foobarbaz |}]
;;

let%expect_test "copy file chmod" =
  let dir = temp_dir () in
  let src = Path.relative dir "initial" in
  let dst = Path.relative dir "final" in
  Io.write_file_exn src "foobarbaz";
  let umask = Unix.umask 0o022 in
  Exn.protect
    ~f:(fun () ->
      Io.copy_file
        ~overwrite:true
        ~chmod:(fun _ ->
          let open Permissions in
          Mode.create ~user:(read + write) ~group:(read + execute) ~other:read ())
        ~src
        ~dst
        ())
    ~finally:(fun () -> ignore (Unix.umask umask : int));
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
  match Io.copy_file ~overwrite:true ~src ~dst () with
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
  Io.copy_file ~overwrite:true ~src ~dst ();
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
  match Io.copy_file ~overwrite:true ~src ~dst () with
  | _ -> assert false
  | exception Sys_error s ->
    let s =
      let _, exn = String.lsplit2_exn s ~on:':' in
      sprintf "$DIR:%s" exn
    in
    print_endline s;
    [%expect {| $DIR: Is a directory |}]
;;
