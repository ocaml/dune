open Stdune
open Dune_scheduler
open Patch_examples

let () = Dune_tests_common.init ()

(* Testing the patch action *)

include struct
  open Dune_engine
  module Action = Action
  module Display = Display
  module Process = Process
end

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

let%expect_test "patching a file" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", basic);
  check "foo.ml";
  [%expect
    {|
    This is right |}]
;;

let%expect_test "patching a file in a subdirectory" =
  test [ "dir/foo.ml", "This is wrong\n" ] ("foo.patch", subdirectory);
  check "dir/foo.ml";
  [%expect
    {|
    This is right |}]
;;

let%expect_test "patching two files with a single patch" =
  test
    [ "foo.ml", "This is wrong\n"; "dir/foo.ml", "This is wrong\n" ]
    ("foo.patch", combined);
  check "foo.ml";
  [%expect
    {|
    This is right |}]
;;

let%expect_test "patching a new file" =
  test [] ("foo.patch", new_file);
  check "foo.ml";
  [%expect
    {|
    This is right |}]
;;

let () = Dune_util.Report_error.report_backtraces true

let%expect_test "patching a deleted file" =
  let filename = "foo.ml" in
  test [ filename, "This is wrong\n" ] ("foo.patch", delete_file);
  (* Different implementations of the patch command behave differently when the
     patch specifies deleting a file: *)
  match Unix.stat filename with
  | { st_kind = S_REG; st_size; _ } ->
    (* Some implementations of patch (e.g. the patch that ships with macOS
       15.1) do not delete files, and instead truncate them to 0 length. If
       the file still exists after applying the patch, assert that it is now
       empty. *)
    assert (st_size == 0)
  | _ -> failwith "Not a regular file"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
    (* Most implementations of patch will delete the file. *)
    ()
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

let%expect_test "Using a patch from 'diff' with a timestamp" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", unified);
  check "foo.ml";
  [%expect
    {|
    This is right |}]
;;

let%expect_test "patching a file without prefix" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", no_prefix);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "patching files with freestyle prefix" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", random_prefix);
  check "foo.ml";
  [%expect {| This is right |}]
;;

let%expect_test "patching files with spaces" =
  try
    test [ "foo bar", "This is wrong\n" ] ("foo.patch", spaces);
    check "foo bar";
    [%expect.unreachable]
  with
  | Dune_util.Report_error.Already_reported ->
    print_endline @@ normalize_error_path [%expect.output];
    [%expect {| Error: foo: No such file or directory |}]
;;

let%expect_test "patching files with (unified) spaces" =
  try
    test [ "foo bar", "This is wrong\n" ] ("foo.patch", unified_spaces);
    check "foo bar";
    [%expect.unreachable]
  with
  | Dune_util.Report_error.Already_reported ->
    print_endline @@ normalize_error_path [%expect.output];
    [%expect {| Error: foo: No such file or directory |}]
;;
