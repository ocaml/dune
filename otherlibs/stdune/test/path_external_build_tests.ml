open Stdune

(* Tests for path operations when the build directory is an external (absolute)
   path. The existing stdune_unit_tests set build dir to "_build" (In_source_dir),
   so the [External b] branch of [Build.to_string] and cross-tree [reach] are
   never exercised there. This library has its own init because [set_root] and
   [Build.set_build_dir] can only be called once per process.

   CR-someday Alizter: After the fix to use [External.append_local] instead of
   [Filename.concat], the conditional Windows expectations can be consolidated
   with the Unix ones. *)

let check_on_win_or_unix output ~wind ~unix =
  let expected = String.trim (if Sys.win32 then wind else unix) in
  let output = String.trim output in
  if not (String.equal output expected)
  then
    Code_error.raise
      "output mismatch"
      [ "expected", String expected; "got", String output ]
;;

let () =
  Printexc.record_backtrace false;
  Path.set_root (Path.External.of_string "/workspace");
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "/external-build")
;;

let%expect_test "Build.to_string root" =
  Path.Build.to_string Path.Build.root |> print_endline;
  [%expect {| /external-build |}]
;;

let%expect_test "Build.to_string relative" =
  Path.Build.to_string (Path.Build.relative Path.Build.root "foo/bar") |> print_endline;
  check_on_win_or_unix
    [%expect.output]
    ~wind:{| /external-build\foo/bar |}
    ~unix:{| /external-build/foo/bar |}
;;

let%expect_test "to_absolute_filename source path" =
  Path.to_absolute_filename (Path.relative Path.root "src/main.ml") |> print_endline;
  [%expect {| /workspace/src/main.ml |}]
;;

let%expect_test "to_absolute_filename build path" =
  Path.to_absolute_filename (Path.relative Path.build_dir "foo/bar") |> print_endline;
  [%expect {| /external-build/foo/bar |}]
;;

let%expect_test "reach build from source" =
  Path.reach (Path.relative Path.build_dir "foo") ~from:Path.root |> print_endline;
  [%expect {| /external-build/foo |}]
;;

let%expect_test "reach source from build" =
  Path.reach (Path.relative Path.root "src") ~from:(Path.relative Path.build_dir "foo")
  |> print_endline;
  [%expect {| /workspace/src |}]
;;

let%expect_test "reach_for_running build from source" =
  Path.reach_for_running (Path.relative Path.build_dir "foo/bar") ~from:Path.root
  |> print_endline;
  [%expect {| /external-build/foo/bar |}]
;;
