open Stdune

let () = Dune_tests_common.init ()

(* Testing the parsing of byte values *)
let parse_bytes value =
  Dune_sexp.Ast.atom_or_quoted_string Loc.none value
  |> Dune_sexp.Decoder.parse Dune_sexp.Decoder.bytes_unit Univ_map.empty
;;

let rec long_power (l : int64) (n : int) : int64 =
  if n = 0 then 1L else Int64.mul l @@ long_power l (n - 1)
;;

let parse_and_assert ?check value =
  let value = parse_bytes value in
  (match check with
   | None -> ()
   | Some check -> assert (value = check));
  value
;;

let test_bytes ?check value = parse_and_assert ?check value |> Printf.printf "%#Ld\n"

(* Hack to insert underscores for hex values. Digits must only be 0-9 *)
let test_bytes_hex ?check value =
  match parse_and_assert ?check value |> sprintf "%Lx" |> Int.of_string with
  | Some x -> x |> Printf.sprintf "0x%#d\n" |> print_endline
  | None -> print_endline "hex value must not have letters"
;;

(* Test parsing of integers. *)

let%expect_test "parsing no suffix" =
  try test_bytes "100" with
  | exn ->
    User_message.print (User_message.make [ Exn.pp exn ]);
    [%expect
      {|
      File "<none>", line 1, characters 0-0:
      Error: missing suffix, use one of B, kB, KiB, MB, MiB, GB, GiB, TB, TiB |}]
;;

(* Test all suffixes. We print binary units in hex to better see output. *)

let%expect_test "parsing B suffix" =
  test_bytes "1B" ~check:(long_power 1024L 0);
  [%expect {| 1 |}]
;;

let%expect_test "parsing kB suffix" =
  test_bytes "1kB" ~check:(long_power 1000L 1);
  [%expect {| 1_000 |}]
;;

let%expect_test "parsing KiB suffix" =
  test_bytes_hex "1KiB" ~check:(long_power 1024L 1);
  [%expect {| 0x400 |}]
;;

let%expect_test "parsing MB suffix" =
  test_bytes "1MB" ~check:(long_power 1000L 2);
  [%expect {| 1_000_000 |}]
;;

let%expect_test "parsing MiB suffix" =
  test_bytes_hex "1MiB" ~check:(long_power 1024L 2);
  [%expect {| 0x100_000 |}]
;;

let%expect_test "parsing GB suffix" =
  test_bytes "1GB" ~check:(long_power 1000L 3);
  [%expect {| 1_000_000_000 |}]
;;

let%expect_test "parsing GiB suffix" =
  test_bytes_hex "1GiB" ~check:(long_power 1024L 3);
  [%expect {| 0x40_000_000 |}]
;;

let%expect_test "parsing TB suffix" =
  test_bytes "1TB" ~check:(long_power 1000L 4);
  [%expect {| 1_000_000_000_000 |}]
;;

let%expect_test "parsing TiB suffix" =
  test_bytes_hex "1TiB" ~check:(long_power 1024L 4);
  [%expect {| 0x10_000_000_000 |}]
;;

let%expect_test "deleted extension version" =
  let supported_versions =
    [ (0, 1), `Since (1, 0); (0, 1), `Deleted_in (3, 24); (0, 2), `Since (3, 24) ]
  in
  let syntax =
    Dune_sexp.Syntax.create
      ~name:(Dune_sexp.Syntax.Name.parse "test-extension")
      ~desc:"the test extension"
      supported_versions
  in
  Dune_sexp.Syntax.check_supported ~dune_lang_ver:(3, 23) syntax (Loc.none, (0, 1));
  (try
     Dune_sexp.Syntax.check_supported ~dune_lang_ver:(3, 24) syntax (Loc.none, (0, 1))
   with
   | User_error.E message -> User_message.print message);
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Version 0.1 of the test-extension extension has been deleted in Dune
    3.24. Please port this project to a newer version of the extension, such as
    0.2. |}]
;;
