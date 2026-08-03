open Stdune

(* Tests for path operations when the build directory is an external (absolute)
   path. The existing stdune_unit_tests set build dir to "_build" (In_source_dir),
   so the [External b] branch of [Build.to_string] and cross-tree [reach] are
   never exercised there. This library has its own init because [set_root] and
   [Build.set_build_dir] can only be called once per process. *)

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
  [%expect {| /external-build/foo/bar |}]
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
