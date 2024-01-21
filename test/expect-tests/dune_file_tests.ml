open Dune_rules
open! Stdune
open Dune_tests_common

let () = init ()

(* Dune_file.Executables.Link_mode.decode *)
let test s =
  Dune_lang.Decoder.parse
    Executables.Link_mode.decode
    Univ_map.empty
    (Dune_lang.Parser.parse_string ~fname:"" ~mode:Dune_lang.Parser.Mode.Single s)
  |> Executables.Link_mode.to_dyn
  |> print_dyn
;;

let%expect_test _ =
  (* Link modes can be read as a (<mode> <kind>) list *)
  test "(best exe)";
  [%expect {|
Other { mode = best; kind = exe }
|}]
;;

let%expect_test _ =
  (* Some shortcuts also exist *)
  test "exe";
  [%expect {|
Other { mode = best; kind = exe }
|}]
;;

let%expect_test _ =
  test "object";
  [%expect {|
Other { mode = best; kind = object }
|}]
;;

let%expect_test _ =
  test "shared_object";
  [%expect {|
Other { mode = best; kind = shared_object }
|}]
;;

let%expect_test _ =
  test "byte";
  [%expect {|
Other { mode = byte; kind = exe }
|}]
;;

let%expect_test _ =
  test "native";
  [%expect {|
Other { mode = native; kind = exe }
|}]
;;

(* Dune_file.Executables.Link_mode.encode *)
let test l = Executables.Link_mode.encode l

let%expect_test _ =
  (* In the general case, modes are serialized as a list *)
  test (Other { kind = Shared_object; mode = Byte }) |> Dune_lang.to_dyn |> print_dyn;
  [%expect {|
[ "byte"; "shared_object" ]
|}]
;;

(* But the specialized ones are serialized in the minimal version *)
let%expect_test _ =
  test Executables.Link_mode.exe |> Dune_lang.to_dyn |> print_dyn;
  [%expect {|
"exe"
|}]
;;
