open Stdune

let () = Dune_tests_common.init ()

(* Basic example adding and removing a line. *)
let basic =
  {|
diff --git a/foo.ml b/foo.ml
index b69a69a5a..ea988f6bd 100644
--- a/foo.ml
+++ b/foo.ml
@@ -1,2 +1,2 @@
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
@@ -1,2 +1,2 @@
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

(* Testing the patch action *)

include struct
  open Dune_engine
  module Action = Action
  module Display = Display
  module Process = Process
  module Scheduler = Scheduler
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
  Scheduler.Run.go config ~timeout:5.0 ~file_watcher:No_watcher ~on_event:(fun _ _ -> ())
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
  [%expect {|
    This is right |}]
;;

let%expect_test "patching a file in a subdirectory" =
  test [ "dir/foo.ml", "This is wrong\n" ] ("foo.patch", subdirectory);
  check "dir/foo.ml";
  [%expect {|
    This is right |}]
;;

let%expect_test "patching two files with a single patch" =
  test
    [ "foo.ml", "This is wrong\n"; "dir/foo.ml", "This is wrong\n" ]
    ("foo.patch", combined);
  check "foo.ml";
  [%expect {|
    This is right |}]
;;

let%expect_test "patching a new file" =
  test [] ("foo.patch", new_file);
  check "foo.ml";
  [%expect {|
    This is right |}]
;;

let () = Dune_util.Report_error.report_backtraces true

let%expect_test "patching a deleted file" =
  test [ "foo.ml", "This is wrong\n" ] ("foo.patch", delete_file);
  check "foo.ml";
  [%expect {|
    File foo.ml not found |}]
;;
