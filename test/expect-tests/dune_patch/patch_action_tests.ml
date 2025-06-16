open Stdune

let () = Dune_tests_common.init ()

(* Testing the patch action *)

open Dune_scheduler

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
  Dune_patch.For_tests.exec
    ~loc:(Loc.in_file (Path.of_string "dune.patch.test"))
    ~patch:patch_file
    ~dir
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
  | _ -> failwith "Still exists"
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
    [%expect {| Error: Cannot edit file "foo": file does not exist |}]
;;

let%expect_test "action test - unified_spaces" =
  test [ "foo bar", "This is wrong\n" ] ("foo.patch", Patch_examples.unified_spaces);
  check "foo bar";
  [%expect {| This is right |}]
;;

let%expect_test "action test - git_ext_delete_only" =
  test [ "foo.ml", "Hello World\n" ] ("foo.patch", Patch_examples.git_ext_delete_only);
  check "foo.ml";
  [%expect {| File foo.ml not found |}]
;;

let%expect_test "action test - git_ext_create_only" =
  test [] ("foo.patch", Patch_examples.git_ext_create_only);
  check "foo.ml";
  [%expect {| Hello World |}]
;;

let%expect_test "action test - rename_patch" =
  test [ "old.ml", "content\n" ] ("foo.patch", Patch_examples.rename_patch);
  check "new.ml";
  [%expect {| content |}]
;;

let%expect_test "action test - edit_with_rename" =
  test [ "source.ml", "This is wrong\n" ] ("foo.patch", Patch_examples.edit_with_rename);
  check "target.ml";
  [%expect {| This is right |}]
;;
