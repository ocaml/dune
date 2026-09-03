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

let%expect_test "repeat1 returns a nonempty list" =
  let values =
    List.map [ "one"; "two" ] ~f:(Dune_sexp.Ast.atom_or_quoted_string Loc.none)
  in
  let value = Dune_sexp.Ast.List (Loc.none, values) in
  let decoder = Dune_sexp.Decoder.(enter (repeat1 string)) in
  let values : string Nonempty_list.t =
    Dune_sexp.Decoder.parse decoder Univ_map.empty value
  in
  Nonempty_list.iter values ~f:print_endline;
  [%expect
    {|
    one
    two |}]
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
    [ (0, 1), `Since (1, 0); (0, 1), `Deleted_in (3, 25); (0, 2), `Since (3, 25) ]
  in
  let syntax =
    Dune_sexp.Syntax.create
      ~name:(Dune_sexp.Syntax.Name.parse "test-extension")
      ~desc:"the test extension"
      supported_versions
  in
  Dune_sexp.Syntax.check_supported ~dune_lang_ver:(3, 24) syntax (Loc.none, (0, 1));
  (try
     Dune_sexp.Syntax.check_supported ~dune_lang_ver:(3, 25) syntax (Loc.none, (0, 1))
   with
   | User_error.E message -> User_message.print message);
  [%expect
    {|
    File "<none>", line 1, characters 0-0:
    Error: Version 0.1 of the test-extension extension has been deleted in Dune
    3.25. Please port this project to a newer version of the extension, such as
    0.2. |}]
;;

let quoted value = Dune_sexp.to_string (Quoted_string value)

let escaped value =
  let quoted = quoted value in
  String.sub quoted ~pos:1 ~len:(String.length quoted - 2)
;;

let print_escaped value = Printf.printf "%S -> %S\n" value (escaped value)

let%expect_test "escaped - plain strings pass through unchanged" =
  print_escaped "hello";
  print_escaped "foo_bar";
  print_escaped "123";
  print_escaped "a/b/c";
  [%expect
    {|
    "hello" -> "hello"
    "foo_bar" -> "foo_bar"
    "123" -> "123"
    "a/b/c" -> "a/b/c"
    |}]
;;

let%expect_test "escaped - special characters are escaped" =
  print_escaped "has\"quote";
  print_escaped "back\\slash";
  print_escaped "new\nline";
  print_escaped "tab\there";
  print_escaped "car\rret";
  print_escaped "back\bspace";
  [%expect
    {|
    "has\"quote" -> "has\\\"quote"
    "back\\slash" -> "back\\\\slash"
    "new\nline" -> "new\\nline"
    "tab\there" -> "tab\\there"
    "car\rret" -> "car\\rret"
    "back\bspace" -> "back\\bspace"
    |}]
;;

let%expect_test "escaped - percent brace escaping" =
  print_escaped "%{var}";
  print_escaped "100%";
  print_escaped "%%";
  print_escaped "%alone";
  [%expect
    {|
    "%{var}" -> "\\%{var}"
    "100%" -> "100%"
    "%%" -> "%%"
    "%alone" -> "%alone"
    |}]
;;

let%expect_test "escaped - empty string" =
  let result = escaped "" in
  Printf.printf "%S -> %S\n" "" result;
  [%expect {| "" -> "" |}]
;;

let%expect_test "escaped - non-ascii bytes are octal-escaped" =
  print_escaped "\x00";
  print_escaped "\x01";
  print_escaped "\x7f";
  print_escaped "\xff";
  [%expect
    {|
    "\000" -> "\000"
    "\001" -> "\001"
    "\127" -> "\127"
    "\255" -> "\\255"
    |}]
;;

let%expect_test "escaped - valid utf8 passes through" =
  (* 2-byte: é *)
  print_escaped "\xc3\xa9";
  (* 3-byte: € *)
  print_escaped "\xe2\x82\xac";
  (* 4-byte: 𝄞 *)
  print_escaped "\xf0\x9d\x84\x9e";
  [%expect
    {|
    "\195\169" -> "\195\169"
    "\226\130\172" -> "\226\130\172"
    "\240\157\132\158" -> "\240\157\132\158"
    |}]
;;

let%expect_test "quoted - wraps in double quotes" =
  let test value = Printf.printf "%S -> %s\n" value (quoted value) in
  test "";
  test "hello";
  test "has space";
  test "has\"quote";
  test "new\nline";
  test "%{var}";
  [%expect
    {|
    "" -> ""
    "hello" -> "hello"
    "has space" -> "has space"
    "has\"quote" -> "has\"quote"
    "new\nline" -> "new\nline"
    "%{var}" -> "\%{var}"
    |}]
;;

let%expect_test "escaped lengths match expected values" =
  let test value expected =
    let actual = String.length (escaped value) in
    if expected <> actual
    then Printf.printf "MISMATCH %S: expected=%d actual=%d\n" value expected actual
  in
  test "" 0;
  test "hello" 5;
  test "has\"quote" 10;
  test "new\nline" 9;
  test "%{var}" 7;
  test "\x00\xff" 5;
  test "\xc3\xa9" 2;
  test "\xe2\x82\xac" 3;
  test "\xf0\x9d\x84\x9e" 4;
  print_endline "all match";
  [%expect {| all match |}]
;;

let%expect_test "escaped - mixed content" =
  print_escaped "hello\nworld\t!";
  print_escaped "say \"hi\" and \\go";
  print_escaped "%{x} is 100%";
  [%expect
    {|
    "hello\nworld\t!" -> "hello\\nworld\\t!"
    "say \"hi\" and \\go" -> "say \\\"hi\\\" and \\\\go"
    "%{x} is 100%" -> "\\%{x} is 100%"
    |}]
;;
