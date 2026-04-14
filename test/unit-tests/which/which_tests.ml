(* Test that Which.candidates handles programs that already have .exe extension.
   This is a regression test for https://github.com/ocaml/dune/pull/13620

   On Windows, if a program name already has .exe, we should not produce
   "prog.exe.exe". This test only runs on Windows (via enabled_if in dune). *)

let test prog =
  Dune_rules.For_tests.Which.candidates prog
  |> Dyn.list Dyn.string
  |> Dyn.to_string
  |> Printf.printf "candidates %S = %s\n" prog
;;

(* See the .expected file for the output *)
let () =
  (* Standard case: program without .exe gets .exe added *)
  test "ocamlc";
  (* The key test: "ocamlc.exe" should NOT produce "ocamlc.exe.exe" *)
  test "ocamlc.exe";
  (* Case insensitive check *)
  test "OCAMLC.EXE";
  (* Non-special program (not in programs_for_which_we_prefer_opt_ext) *)
  test "foo";
  (* Non-special program with .exe should not get double extension *)
  test "foo.exe"
;;
