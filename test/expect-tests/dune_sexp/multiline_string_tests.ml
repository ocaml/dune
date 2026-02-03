open Stdune

let feature_ver = 3, 25
let before_feature_ver = 3, 24
let parse_csts input = Dune_sexp.Parser.parse_string ~fname:"test" ~mode:Cst input

let print_csts input =
  List.iter (parse_csts input) ~f:(fun cst ->
    print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string))
;;

let print_abstract_csts input =
  List.iter (parse_csts input) ~f:(fun cst ->
    match Dune_sexp.Cst.abstract cst with
    | None -> print_endline "None"
    | Some ast ->
      let sexp = Dune_sexp.Ast.remove_locs ast in
      print_endline (Dune_sexp.to_dyn sexp |> Dyn.to_string))
;;

let ast_without_locs input =
  Dune_sexp.Parser.parse_string ~fname:"test" ~mode:Single input
  |> Dune_sexp.Ast.remove_locs
;;

let format_round_trip ~version input =
  let formatted = Dune_lang.Format.format_string ~version input in
  let reformatted = Dune_lang.Format.format_string ~version formatted in
  let before = Dune_sexp.to_dyn (ast_without_locs input) |> Dyn.to_string in
  let after = Dune_sexp.to_dyn (ast_without_locs formatted) |> Dyn.to_string in
  if not (String.equal before after)
  then
    Code_error.raise
      "formatting changed block string semantics"
      [ "before", Dyn.string before; "after", Dyn.string after ];
  if not (String.equal formatted reformatted)
  then
    Code_error.raise
      "block string formatting is not idempotent"
      [ "formatted", Dyn.string formatted; "reformatted", Dyn.string reformatted ];
  formatted
;;

let format_feature input = format_round_trip ~version:feature_ver input

let print_format_round_trip input =
  format_feature input |> Printf.printf "formatted: %S\n"
;;

(* ==================== Parsing Tests ====================
   These tests verify the data structure produced by parsing block strings. *)

let%expect_test "parse: basic block string" =
  let input =
    {|"\| hello
"\| world
|}
  in
  print_csts input;
  [%expect
    {|
    Block_string
      [ (Escaped, [ Text "hello" ]); (Escaped, [ Text "world" ]); (Escaped, []) ]
    |}]
;;

let%expect_test "parse: block string in list" =
  let input =
    {|(echo "\| hello
      "\| world
)|}
  in
  print_csts input;
  [%expect
    {|
    List
      [ Atom (A "echo")
      ; Block_string
          [ (Escaped, [ Text "hello" ])
          ; (Escaped, [ Text "world" ])
          ; (Escaped, [])
          ]
      ]
    |}]
;;

let%expect_test "parse: mixed block kinds (escaped and raw)" =
  let input =
    {|"\| first
"\> second
"\| third
|}
  in
  print_csts input;
  [%expect
    {|
    Block_string
      [ (Escaped, [ Text "first" ])
      ; (Raw, [ Text "second" ])
      ; (Escaped, [ Text "third" ])
      ; (Escaped, [])
      ]
    |}]
;;

let%expect_test "parse: empty block string" =
  let input =
    {|"\|
|}
  in
  print_csts input;
  [%expect {| Block_string [ (Escaped, []); (Escaped, []) ] |}]
;;

let%expect_test "parse: block string with pform" =
  let input =
    {|"\| hello %{name}
"\| world
|}
  in
  print_csts input;
  [%expect
    {|
    Block_string
      [ (Escaped, [ Text "hello "; Pform { name = "name"; payload = None } ])
      ; (Escaped, [ Text "world" ])
      ; (Escaped, [])
      ]
    |}]
;;

let%expect_test "parse: \\n escape in block string creates line break" =
  let input =
    {|"\| line one\nline two
|}
  in
  print_csts input;
  [%expect
    {|
    Block_string
      [ (Escaped, [ Text "line one" ])
      ; (Escaped, [ Text "line two" ])
      ; (Escaped, [])
      ]
    |}]
;;

let%expect_test "parse: raw block string preserves backslash-n literal" =
  let input =
    {|"\> echo \n something
|}
  in
  print_csts input;
  [%expect {| Block_string [ (Raw, [ Text "echo \\n something" ]); (Raw, []) ] |}]
;;

(* ==================== Round-trip / Formatting Tests ====================
   These tests verify that formatting produces correct output and is idempotent. *)

let%expect_test "format: block string with feature version" =
  let input =
    {|"\| hello
"\| world
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\| hello
    "\| world
  |}]
;;

let%expect_test "format: block string with older version (fallback)" =
  let input =
    {|"\| hello
"\| world
|}
  in
  let output = format_round_trip ~version:before_feature_ver input in
  print_endline output;
  [%expect {| "hello\nworld\n" |}]
;;

let%expect_test "format: escaped pforms with older version" =
  let input =
    {|"\| %{name}
|}
  in
  format_round_trip ~version:before_feature_ver input |> print_string;
  [%expect {| "%{name}\n" |}]
;;

let%expect_test "format: raw pform-like text with older version" =
  let input =
    {|"\> %{name}
|}
  in
  format_round_trip ~version:before_feature_ver input |> print_string;
  [%expect {| "\%{name}\n" |}]
;;

let%expect_test "format: mixed block kinds preserves each line's kind" =
  let input =
    {|"\> first
"\| second
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\> first
    "\| second
    |}]
;;

let%expect_test "format: empty block string" =
  let input =
    {|"\|
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\|
  |}]
;;

let%expect_test "format: block string with pform" =
  let input =
    {|"\| hello %{name}
"\| world
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\| hello %{name}
    "\| world
    |}]
;;

let%expect_test "format: \\n escape formats as multi-line" =
  let input =
    {|"\| line one\nline two
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\| line one
    "\| line two
    |}]
;;

let%expect_test "format: raw block string preserves backslash-n" =
  let input =
    {|"\> echo \n something
|}
  in
  let output = format_feature input in
  print_endline output;
  [%expect
    {|
    "\> echo \n something
    |}]
;;

(* ==================== CST to AST Conversion Tests ====================
   These tests verify that block strings with pforms are correctly converted
   to Templates when abstracting CST to AST. *)

let%expect_test "abstract: block string without pforms becomes Quoted_string" =
  let input =
    {|"\| hello world
|}
  in
  print_abstract_csts input;
  [%expect
    {|
    "hello world\n\
     "
    |}]
;;

let%expect_test "abstract: block string with pform becomes Template" =
  let input =
    {|"\| hello %{name}
|}
  in
  print_abstract_csts input;
  (* Block strings with pforms become Templates so the pforms get expanded *)
  [%expect {| template "\"hello %{name}\\n\"" |}]
;;

let%expect_test "round-trip: escaped block string contents" =
  print_format_round_trip
    {|"\| \%{literal} \\n
|};
  [%expect {| formatted: "\"\\| \\%{literal} \\\\n\n" |}]
;;

let%expect_test "round-trip: escaped continuation before a raw line" =
  print_format_round_trip
    {|"\| %{x}\
"\> literal
|};
  [%expect {| formatted: "\"\\| %{x}literal\n" |}]
;;

let%expect_test "round-trip: CRLF block string" =
  print_format_round_trip "\"\\| value\r\n";
  [%expect {| formatted: "\"\\| value\\r\n" |}]
;;

let%expect_test "round-trip: block string without a final newline" =
  print_format_round_trip {|"\| value|};
  [%expect {| formatted: "\"value\"\n" |}]
;;

let%expect_test "format: empty block string without a final newline" =
  format_feature {|"\| |} |> Printf.printf "%S\n";
  [%expect {| "\"\"\n" |}]
;;

let%expect_test "format: block string nested in singleton lists" =
  format_feature
    {|(("\| value
"\> next
))|}
  |> print_string;
  [%expect
    {|
    (("\| value
      "\> next
      ))
    |}]
;;

let%expect_test "format: numeric newline escape in a block string" =
  format_feature
    {|"\| before\010after
|}
  |> Printf.printf "%S\n";
  [%expect {| "\"\\| before\n\"\\| after\n" |}]
;;

let%expect_test "round-trip: raw CRLF block string" =
  print_format_round_trip "\"\\> %{literal}\r\n";
  [%expect {| formatted: "\"\\| \\%{literal}\\r\n" |}]
;;

let%expect_test "round-trip: raw block string without a final newline" =
  print_format_round_trip {|"\> value|};
  [%expect {| formatted: "\"value\"\n" |}]
;;

let%expect_test "round-trip: consecutive block strings" =
  print_format_round_trip
    {|(items "\| first

"\> second
)|};
  [%expect {| formatted: "(items\n \"\\| first\n \n \"\\> second\n )\n" |}]
;;

let%expect_test "format: block string followed by another list element" =
  format_feature
    {|(items "\| first
"\| second
tail)|}
  |> print_string;
  [%expect
    {|
    (items
     "\| first
     "\| second
     tail)
    |}]
;;
