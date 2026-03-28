open Stdune

(* Version constants for block string formatting *)
let feature_ver = 3, 23
let before_feature_ver = 3, 22

(* ==================== Parsing Tests ====================
   These tests verify the data structure produced by parsing block strings. *)

let%expect_test "parse: basic block string" =
  let input =
    {|"\| hello
"\| world
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "hello" ])
         ; (Escaped, [ Text "world" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: block string in list" =
  let input =
    {|(echo "\| hello
      "\| world
)|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    List
      [ Atom (A "echo")
      ; Quoted_string
          (Multi
             [ (Escaped, [ Text "hello" ])
             ; (Escaped, [ Text "world" ])
             ; (Escaped, [])
             ])
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
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "first" ])
         ; (Raw, [ Text "second" ])
         ; (Escaped, [ Text "third" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: empty block string" =
  let input =
    {|"\|
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect {| Quoted_string (Multi [ (Escaped, []); (Escaped, []) ]) |}]
;;

let%expect_test "parse: block string with pform" =
  let input =
    {|"\| hello %{name}
"\| world
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "hello "; Pform { name = "name"; payload = None } ])
         ; (Escaped, [ Text "world" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: block string with pform and mixed kinds" =
  let input =
    {|"\| hello %{name}
"\> raw %{value}
"\| back to escaped
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "hello "; Pform { name = "name"; payload = None } ])
         ; (Raw, [ Text "raw "; Pform { name = "value"; payload = None } ])
         ; (Escaped, [ Text "back to escaped" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: \\n escape in block string creates line break" =
  let input =
    {|"\| line one\nline two
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "line one" ])
         ; (Escaped, [ Text "line two" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: multiple \\n escapes create multiple lines" =
  let input =
    {|"\| a\nb\nc
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped, [ Text "a" ])
         ; (Escaped, [ Text "b" ])
         ; (Escaped, [ Text "c" ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: raw block string preserves backslash-n literal" =
  let input =
    {|"\> echo \n something
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {| Quoted_string (Multi [ (Raw, [ Text "echo \\n something" ]); (Raw, []) ]) |}]
;;

let%expect_test "parse: regular quoted string \\n embeds newline" =
  let input = {|"line one\nline two"|} in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string (Single "line one\n\
                           line two")
    |}]
;;

(* ==================== Quoted_string.to_string Tests ==================== *)

let%expect_test "to_string: block string" =
  let open Dune_sexp.Quoted_string.Block_kind in
  let open Dune_sexp.Part in
  let qs =
    Dune_sexp.Quoted_string.Multi
      [ Escaped, [ Text "hello" ]; Escaped, [ Text "world" ]; Escaped, [] ]
  in
  print_endline (Dune_sexp.Quoted_string.to_string qs);
  [%expect
    {|
    hello
    world
    |}]
;;

(* ==================== Round-trip / Formatting Tests ====================
   These tests verify that formatting produces correct output and is idempotent. *)

let%expect_test "format: block string with feature version" =
  let input =
    {|"\| hello
"\| world
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
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
  let output = Dune_lang.Format.format_string ~version:before_feature_ver input in
  print_endline output;
  [%expect {| "hello\nworld\n" |}]
;;

let%expect_test "format: mixed block kinds preserves each line's kind" =
  let input =
    {|"\> first
"\| second
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
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
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
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
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
  print_endline output;
  [%expect
    {|
    "\| hello %{name}
    "\| world
    |}]
;;

let%expect_test "format: block string with pform and mixed kinds" =
  let input =
    {|"\| hello %{name}
"\> raw %{value}
"\| back to escaped
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
  print_endline output;
  [%expect
    {|
    "\| hello %{name}
    "\> raw %{value}
    "\| back to escaped
    |}]
;;

let%expect_test "format: \\n escape formats as multi-line" =
  let input =
    {|"\| line one\nline two
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
  print_endline output;
  [%expect
    {|
    "\| line one
    "\| line two
    |}]
;;

let%expect_test "format: multiple \\n escapes" =
  let input =
    {|"\| a\nb\nc
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
  print_endline output;
  [%expect
    {|
    "\| a
    "\| b
    "\| c
    |}]
;;

let%expect_test "format: raw block string preserves backslash-n" =
  let input =
    {|"\> echo \n something
|}
  in
  let output = Dune_lang.Format.format_string ~version:feature_ver input in
  print_endline output;
  [%expect
    {|
    "\> echo \n something
    |}]
;;

let%expect_test "round-trip: basic block string" =
  let input =
    {|"\| hello
"\| world
|}
  in
  let output1 = Dune_lang.Format.format_string ~version:feature_ver input in
  let output2 = Dune_lang.Format.format_string ~version:feature_ver output1 in
  assert (String.equal output1 output2);
  print_endline output1;
  [%expect
    {|
    "\| hello
    "\| world
    |}]
;;

let%expect_test "round-trip: mixed kinds" =
  let input =
    {|"\> raw line
"\| escaped line
|}
  in
  let output1 = Dune_lang.Format.format_string ~version:feature_ver input in
  let output2 = Dune_lang.Format.format_string ~version:feature_ver output1 in
  assert (String.equal output1 output2);
  print_endline output1;
  [%expect
    {|
    "\> raw line
    "\| escaped line
    |}]
;;

(* ==================== Macros with Payloads Tests ====================
   These tests verify that macros (pforms with payloads) work correctly in block strings. *)

let%expect_test "parse: block string with macro" =
  let input =
    {|"\| contents: %{read:file.txt}
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Escaped,
            [ Text "contents: "
            ; Pform { name = "read"; payload = Some "file.txt" }
            ])
         ; (Escaped, [])
         ])
    |}]
;;

let%expect_test "parse: raw block string with macro" =
  let input =
    {|"\> cat %{read:file.txt} | grep foo
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst -> print_endline (Dune_sexp.Cst.to_dyn cst |> Dyn.to_string));
  [%expect
    {|
    Quoted_string
      (Multi
         [ (Raw,
            [ Text "cat "
            ; Pform { name = "read"; payload = Some "file.txt" }
            ; Text " | grep foo"
            ])
         ; (Raw, [])
         ])
    |}]
;;

let%expect_test "format: block string with macro (older version fallback)" =
  let input =
    {|"\| contents: %{read:file.txt}
|}
  in
  let output = Dune_lang.Format.format_string ~version:before_feature_ver input in
  print_endline output;
  [%expect {| "contents: %{read:file.txt}\n" |}]
;;

let%expect_test "round-trip: block string with macros" =
  let input =
    {|"\| start %{read:a.txt}
"\> raw %{env:PATH}
"\| end %{read:b.txt}
|}
  in
  let output1 = Dune_lang.Format.format_string ~version:feature_ver input in
  let output2 = Dune_lang.Format.format_string ~version:feature_ver output1 in
  assert (String.equal output1 output2);
  print_endline output1;
  [%expect
    {|
    "\| start %{read:a.txt}
    "\> raw %{env:PATH}
    "\| end %{read:b.txt}
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
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst ->
    match Dune_sexp.Cst.abstract cst with
    | None -> print_endline "None"
    | Some ast ->
      let t = Dune_sexp.Ast.remove_locs ast in
      print_endline (Dune_sexp.to_dyn t |> Dyn.to_string));
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
  let lb = Lexbuf.from_string ~fname:"test" input in
  let csts = Dune_sexp.Parser.parse lb ~mode:Cst in
  List.iter csts ~f:(fun cst ->
    match Dune_sexp.Cst.abstract cst with
    | None -> print_endline "None"
    | Some ast ->
      let t = Dune_sexp.Ast.remove_locs ast in
      print_endline (Dune_sexp.to_dyn t |> Dyn.to_string));
  (* Block strings with pforms become Templates so the pforms get expanded *)
  [%expect {| template "\"hello %{name}\\n\"" |}]
;;

let%expect_test "parse Single mode: block string with pform becomes Template" =
  (* This tests the Parser's internal Encoded.to_ast path, which is used
     when parsing in Single/Many/Many_as_one modes for decoding *)
  let input =
    {|"\| hello %{name}
|}
  in
  let lb = Lexbuf.from_string ~fname:"test" input in
  let ast = Dune_sexp.Parser.parse lb ~mode:Single in
  let t = Dune_sexp.Ast.remove_locs ast in
  print_endline (Dune_sexp.to_dyn t |> Dyn.to_string);
  (* Block strings with pforms become Templates so the pforms get expanded *)
  [%expect {| template "\"hello %{name}\\n\"" |}]
;;
