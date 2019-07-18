open Dune
open! Stdune

let print pp = Format.printf "%a@." Pp.render_ignore_tags pp
let print_dyn dyn = print (Dyn.pp dyn)

(* Dune_file.Executables.Link_mode.decode *)
let test s =
  Dune_lang.Decoder.parse Dune_file.Executables.Link_mode.decode Univ_map.empty
    (Dune_lang.parse_string ~fname:"" ~mode:Dune_lang.Parser.Mode.Single s)
  |> Dune_file.Executables.Link_mode.to_dyn
  |> print_dyn

let%expect_test _ =
  (* Link modes can be read as a (<mode> <kind>) list *)
  test "(best exe)";
  [%expect{|
{mode = best;
  kind = exe}
|}]

let%expect_test _ =
  (* Some shortcuts also exist *)
  test "exe";
  [%expect{|
{mode = best;
  kind = exe}
|}]

let%expect_test _ =
  test "object";
  [%expect{|
{mode = best;
  kind = object}
|}]

let%expect_test _ =
  test "shared_object";
  [%expect{|
{mode = best;
  kind = shared_object}
|}]

let%expect_test _ =
  test "byte";
  [%expect{|
{mode = byte;
  kind = exe}
|}]

let%expect_test _ =
  test "native";
  [%expect{|
{mode = native;
  kind = exe}
|}]

(* Dune_file.Executables.Link_mode.encode *)
let test l =
  Dune_file.Executables.Link_mode.encode l

let%expect_test _ =
  (* In the general case, modes are serialized as a list *)
  test { Dune_file.Executables.Link_mode.kind = Shared_object
       ; mode = Byte
       ; loc=Loc.none
       }
  |> Dune_lang.to_dyn
  |> print_dyn;
  [%expect{|
["byte"; "shared_object"]
|}]

(* But the specialized ones are serialized in the minimal version *)
let%expect_test _=
  test Dune_file.Executables.Link_mode.exe
  |> Dune_lang.to_dyn
  |> print_dyn;
  [%expect{|
"exe"
|}]
