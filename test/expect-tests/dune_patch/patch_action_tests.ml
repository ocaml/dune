open Stdune

include struct
  open Dune_engine
  module Display = Display
  module Process = Process
end

open Dune_scheduler

let () = Dune_tests_common.init ()

let create_files =
  List.iter ~f:(fun (f, contents) ->
    ignore
      (Fpath.mkdir_p
         (Path.Local.of_string f
          |> Path.Local.parent
          |> Option.value ~default:(Path.Local.of_string ".")
          |> Path.Local.to_string));
    Io.String_path.write_file f contents)
;;

let test files (patch, patch_contents) =
  let dir = Temp.create Dir ~prefix:"dune" ~suffix:"patch_test" in
  let display = Display.Quiet in
  Sys.chdir (Path.to_string dir);
  let patch_file = Path.append_local dir (Path.Local.of_string patch) in
  let config =
    { Scheduler.Config.concurrency = 1
    ; print_ctrl_c_warning = false
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go
    config
    ~timeout:(Time.Span.of_secs 5.0)
    ~file_watcher:No_watcher
    ~on_event:(fun _ -> ())
  @@ fun () ->
  let open Fiber.O in
  let* () = Fiber.return @@ create_files ((patch, patch_contents) :: files) in
  Dune_patch.For_tests.exec display ~patch:patch_file ~dir ~stderr:Process.Io.stderr
;;

let check path =
  match (Unix.stat path).st_kind with
  | S_REG -> Io.String_path.cat path
  | _ -> failwith "Not a regular file"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> printfn "File %s not found" path
;;

let%expect_test "action test - basic" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.basic);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - subdirectory" =
  test [ "dir/foo.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.subdirectory);
  check "dir/foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - combined" =
  test
    [ "foo.ml", "This is wrong\n"; "dir/foo.ml", "This is wrong\n" ]
    ("foo.patch", Patch_examples.combined);
  check "foo.ml";
  [%expect {| This is right |}];
  check "dir/foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - new_file" =
  test [] ("foo.patch", Patch_examples.new_file);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - delete_file" =
  let filename = "foo.ml" in
  test [ filename, "This is wrong\n" ] ("foo.patch", Patch_examples.delete_file);
  match Unix.stat filename with
  | { st_kind = S_REG; st_size; _ } -> assert (st_size == 0)
  | _ -> failwith "Not a regular file"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
;;

let undo_breaks =
  String.map ~f:(function
    | '\n' -> ' '
    | c -> c)
;;

let rsplit2_exn s ~on =
  match String.rsplit2 s ~on with
  | Some s -> s
  | None -> Code_error.raise "rsplit2_exn" [ "s", String s; "on", Char on ]
;;

let normalize_error_path s =
  let s = undo_breaks s in
  let location, reason = rsplit2_exn s ~on:':' in
  let prefix, path = String.lsplit2_exn location ~on:' ' in
  let path = Filename.basename path in
  sprintf "%s %s:%s" prefix path reason
;;

let%expect_test "action test - unified" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.unified);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - no_prefix" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.no_prefix);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "action test - random_prefix" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.random_prefix);
  check "foo.ml";
  [%expect {| This is right |}]
;;

(* CR-soon alizter: the prefix_of_patch regex truncates unquoted
   filenames at the first space, so "foo bar" becomes "foo". *)
let%expect_test "action test - spaces" =
  try
    test [ "foo bar", "This is wrong\n" ] ("foo.patch", Patch_examples.spaces);
    check "foo bar";
    [%expect.unreachable]
  with
  | Dune_util.Report_error.Already_reported ->
    print_endline @@ normalize_error_path [%expect.output];
    [%expect {| Error: foo: No such file or directory |}]
;;

(* CR-soon alizter: the old implementation passes the quoted
   filename through the regex which computes the wrong prefix,
   causing the external patch to fail. *)
let%expect_test "action test - unified_spaces" =
  try
    test [ "foo bar", "This is wrong\n" ] ("foo.patch", Patch_examples.unified_spaces);
    check "foo bar";
    [%expect.unreachable]
  with
  | Dune_util.Report_error.Already_reported ->
    print_endline @@ normalize_error_path [%expect.output];
    [%expect {| Error: foo: No such file or directory |}]
;;

let%expect_test "action test - git_ext_delete_only" =
  let filename = "foo.ml" in
  test [ filename, "Hello World\n" ] ("foo.patch", Patch_examples.git_ext_delete_only);
  (* macOS patch truncates to 0 bytes instead of deleting *)
  match Unix.stat filename with
  | { st_kind = S_REG; st_size; _ } -> assert (st_size == 0)
  | _ -> failwith "Not a regular file"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
;;

let%expect_test "action test - git_ext_create_only" =
  test [] ("foo.patch", Patch_examples.git_ext_create_only);
  check "foo.ml";
  [%expect {| Hello World |}]
;;

(* CR-soon alizter: the old implementation doesn't strip the
   prefix correctly for edit-with-rename patches, causing the
   external patch to fail. *)
let%expect_test "action test - edit_with_rename" =
  try
    test [ "source.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.edit_with_rename);
    check "target.ml";
    [%expect.unreachable]
  with
  | Dune_util.Report_error.Already_reported ->
    print_endline @@ normalize_error_path [%expect.output];
    [%expect {| Error: target.ml: No such file or directory |}]
;;
