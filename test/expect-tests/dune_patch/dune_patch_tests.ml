open Stdune
open Dune_scheduler

let () = Dune_tests_common.init ()

(* Basic example adding and removing a line. *)
let basic =
  {|
diff --git a/foo.ml b/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/foo.ml
+++ b/foo.ml
@@ -1,1 +1,1 @@
-This is wrong
+This is right
|}
;;

(* Example adding and removing a line in a file in a subdirectory. *)
let subdirectory =
  {|
diff --git a/dir/foo.ml b/dir/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/dir/foo.ml
+++ b/dir/foo.ml
@@ -1,1 +1,1 @@
-This is wrong
+This is right
|}
;;

(* Previous two example combined into a single patch. *)
let combined = String.concat ~sep:"\n" [ basic; subdirectory ]

(* Example adding a new file. *)
let new_file =
  {|
diff --git a/foo.ml b/foo.ml
new file mode 100644
index 000000000..ea988f6bd
--- /dev/null
+++ b/foo.ml
@@ -0,0 +1,2 @@
+This is right
+
|}
;;

(* Example deleting an existing file. *)
let delete_file =
  {|
diff --git a/foo.ml b/foo.ml
deleted file mode 100644
index ea988f6bd..000000000
--- a/foo.ml
+++ /dev/null
@@ -1,1 +0,0 @@
-This is wrong
|}
;;

(* Use GNU diff 'unified' format instead of 'git diff' *)
let unified =
  {|
diff -u a/foo.ml b/foo.ml
--- a/foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ b/foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

let no_prefix =
  {|
--- foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

let random_prefix =
  {|
diff -u bar/foo.ml baz/foo.ml
--- bar/foo.ml	2024-08-29 17:37:53.114980665 +0200
+++ baz/foo.ml	2024-08-29 17:38:00.243088256 +0200
@@ -1 +1 @@
-This is wrong
+This is right
|}
;;

(* The file is called "foo bar" *)
let spaces =
  {|
diff --git a/foo bar b/foo bar
index ef00db3..88adca3 100644
--- a/foo bar   
+++ b/foo bar   
@@ -1 +1 @@
-This is wrong.
+This is right.
|}
;;

(* The file is called "foo bar" but in unified diff its quoted *)
let unified_spaces =
  {|
--- "a/foo bar"	2024-09-04 10:56:24.139293679 +0200
+++ "b/foo bar"	2024-09-04 10:56:12.519195763 +0200
@@ -1 +1 @@
-This is wrong.
+This is right.
|}
;;

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
    ; stats = None
    ; print_ctrl_c_warning = false
    ; watch_exclusions = []
    }
  in
  Scheduler.Run.go
    config
    ~timeout:(Time.Span.of_secs 5.0)
    ~file_watcher:No_watcher
    ~on_event:(fun _ _ -> ())
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
