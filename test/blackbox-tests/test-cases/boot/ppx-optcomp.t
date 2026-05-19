Testing that the bootstrap preprocessor handles [%%if ocaml_version ...]
blocks.

  $ init_bootstrap

  $ cat > input.ml <<EOF
  > [%%if ocaml_version < (5, 4, 0)]
  > let selected = "lt"
  > [%%if ocaml_version < (999, 0, 0)]
  > let nested = "nested-then"
  > [%%else]
  > let nested = "wrong"
  > [%%endif]
  > [%%else]
  > let selected = "ge"
  > [%%if ocaml_version < (0, 0, 0)]
  > let nested = "wrong"
  > [%%else]
  > let nested = "nested-else"
  > [%%endif]
  > [%%endif]
  > EOF

  $ ocamllex -q -o boot/pps.ml boot/pps.mll

  $ cat > dump_pps.ml <<EOF
  > let read_file fn =
  >   let ic = open_in_bin fn in
  >   let s = really_input_string ic (in_channel_length ic) in
  >   close_in ic;
  >   s
  > ;;
  > let () =
  >   try print_string (Pps.pp (read_file Sys.argv.(1))) with
  >   | Failure message ->
  >     prerr_endline ("Failure: " ^ message);
  >     exit 1
  > EOF

  $ ocamlc -I boot -o dump_pps.exe boot/pps.mli boot/pps.ml dump_pps.ml

  $ cat > expected.ml <<EOF
  > let ocaml_version =
  >   Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun a b c -> a, b, c)
  > ;;
  > let () =
  >   if ocaml_version < (5, 4, 0)
  >   then (
  >     print_endline "let selected = \"lt\"";
  >     print_endline "let nested = \"nested-then\"")
  >   else (
  >     print_endline "let selected = \"ge\"";
  >     print_endline "let nested = \"nested-else\"")
  > EOF

  $ ./dump_pps.exe input.ml > actual
  $ ocaml expected.ml > expected
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected > expected.normalized
  $ diff -u expected.normalized actual.normalized

It also accepts all comparison operators, tolerates whitespace in the header,
and drops false branches with no [%%else].

  $ cat > write_input.ml <<'EOF'
  > let ocaml_version =
  >   Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun a b c -> a, b, c)
  > ;;
  > let previous (a, b, c) =
  >   if c > 0
  >   then a, b, c - 1
  >   else if b > 0
  >   then a, b - 1, 0
  >   else a - 1, 0, 0
  > ;;
  > let next (a, b, c) =
  >   a, b, c + 1
  > ;;
  > let pp_version (a, b, c) =
  >   Printf.sprintf "(%d, %d, %d)" a b c
  > ;;
  > let () =
  >   let current = pp_version ocaml_version in
  >   let previous = pp_version (previous ocaml_version) in
  >   let next = pp_version (next ocaml_version) in
  >   Printf.printf
  >     {|
  > let start = "kept"
  > [%%%%if ocaml_version = %s]
  > let eq = "selected"
  > [%%%%else]
  > let eq = "wrong"
  > [%%%%endif]
  > [%%%%if
  >   ocaml_version
  >   <=
  >   %s
  > ]
  > let le = "selected"
  > [%%%%else]
  > let le = "wrong"
  > [%%%%endif]
  > [%%%%if ocaml_version >= %s]
  > let ge = "selected"
  > [%%%%else]
  > let ge = "wrong"
  > [%%%%endif]
  > [%%%%if ocaml_version <> %s]
  > let ne = "wrong"
  > [%%%%else]
  > let ne = "selected"
  > [%%%%endif]
  > [%%%%if ocaml_version < %s]
  > let lt = "selected"
  > [%%%%endif]
  > [%%%%if ocaml_version > %s]
  > let gt = "selected"
  > [%%%%endif]
  > [%%%%if ocaml_version > %s]
  > let absent = "wrong"
  > [%%%%endif]
  > let finish = "kept"
  > |}
  >       current
  >       current
  >       current
  >       current
  >       next
  >       previous
  >       next
  > EOF

  $ ocaml write_input.ml > input-all-operators.ml
  $ cat > expected-all-operators.ml <<EOF
  > let start = "kept"
  > let eq = "selected"
  > let le = "selected"
  > let ge = "selected"
  > let ne = "selected"
  > let lt = "selected"
  > let gt = "selected"
  > let finish = "kept"
  > EOF
  $ ./dump_pps.exe input-all-operators.ml > actual
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected-all-operators.ml > expected.normalized
  $ diff -u expected.normalized actual.normalized

Literal marker text inside strings and comments is preserved.

  $ cat > literal-markers.ml <<EOF
  > let top_level = "[%%if ocaml_version < (1, 2, 3)]"
  > let top_level_escaped = "\" [%%else]"
  > let top_level_char = '\''
  > let top_level_type (x : 'a) = x
  > let top_level_quoted = {|[%%endif]|}
  > let top_level_tagged = {boot|[%%else]|boot}
  > [%%if ocaml_version < (999, 0, 0)]
  > let nested = "[%%endif]"
  > let nested_escaped = "\" [%%if ocaml_version < (1, 2, 3)]"
  > let nested_char = '\''
  > let nested_type (x : 'a) = x
  > let nested_quoted = {|[%%else]|}
  > let nested_tagged = {boot|[%%endif]|boot}
  > (* outer (* [%%endif] *) [%%else] *)
  > [%%else]
  > let wrong = ()
  > [%%endif]
  > EOF
  $ cat > expected-literal-markers.ml <<EOF
  > let top_level = "[%%if ocaml_version < (1, 2, 3)]"
  > let top_level_escaped = "\" [%%else]"
  > let top_level_char = '\''
  > let top_level_type (x : 'a) = x
  > let top_level_quoted = {|[%%endif]|}
  > let top_level_tagged = {boot|[%%else]|boot}
  > let nested = "[%%endif]"
  > let nested_escaped = "\" [%%if ocaml_version < (1, 2, 3)]"
  > let nested_char = '\''
  > let nested_type (x : 'a) = x
  > let nested_quoted = {|[%%else]|}
  > let nested_tagged = {boot|[%%endif]|boot}
  > (* outer (* [%%endif] *) [%%else] *)
  > EOF
  $ ./dump_pps.exe literal-markers.ml > actual
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected-literal-markers.ml > expected.normalized
  $ diff -u expected.normalized actual.normalized

Literal marker text in the discarded branch does not affect the selected one.

  $ cat > discarded-literals.ml <<EOF
  > [%%if ocaml_version < (999, 0, 0)]
  > let kept = "selected"
  > [%%else]
  > let discarded_string = "[%%endif]"
  > let discarded_escaped = "\" [%%else]"
  > let discarded_char = '\''
  > let discarded_type (x : 'a) = x
  > let discarded_quoted = {|[%%else]|}
  > let discarded_tagged = {boot|partial |boo [%%endif]|boot}
  > (* outer (* [%%endif] *) [%%else] *)
  > [%%endif]
  > EOF
  $ cat > expected-discarded-literals.ml <<EOF
  > let kept = "selected"
  > EOF
  $ ./dump_pps.exe discarded-literals.ml > actual
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected-discarded-literals.ml > expected.normalized
  $ diff -u expected.normalized actual.normalized

Conditionals still strip let%expect_test and let%test_module blocks in the
selected branch.

  $ cat > conditional-tests.ml <<EOF
  > [%%if ocaml_version < (999, 0, 0)]
  > let before = "kept"
  > let%expect_test "stripped" =
  >   let _ = "\" ;; [%%else]" in
  >   let _ = '\'' in
  >   let _ = fun (x : 'a) -> x in
  >   let _ = {|;; [%%endif]|} in
  >   let _ = {boot|[%%else]|boot} in
  >   let _ = 0 (* outer (* ;; [%%endif] *) [%%else] *) in
  >   [%expect {||}]
  > ;;
  > let%test_module "nested" =
  >   (module struct
  >     let _ = ')'
  >     let _ = '\''
  >     let _ = fun (x : 'a) -> x
  >     let _ = "\" ) ;; [%%endif]"
  >     let _ = {|) ;; [%%else]|}
  >     let _ = {boot|[%%else]|boot}
  >     let _ = 0 (* outer (* ) ;; [%%endif] *) [%%else] *)
  >     let%expect_test "inner" =
  >       [%expect {||}]
  >     ;;
  >   end)
  > ;;
  > let after = "kept"
  > [%%else]
  > let wrong = ()
  > [%%endif]
  > EOF
  $ cat > expected-conditional-tests.ml <<EOF
  > let before = "kept"
  > let after = "kept"
  > EOF
  $ ./dump_pps.exe conditional-tests.ml > actual
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected-conditional-tests.ml > expected.normalized
  $ diff -u expected.normalized actual.normalized

let%expect_test bodies may contain nested structure items separated by [;;].

  $ cat > nested-structure-items.ml <<EOF
  > [%%if ocaml_version < (999, 0, 0)]
  > let before = "kept"
  > let%expect_test "nested-structure" =
  >   let module M = struct
  >     let x = 1;;
  >     let y = 2
  >   end in
  >   ignore (M.x, M.y);
  >   [%expect {||}]
  > ;;
  > let after = "kept"
  > [%%else]
  > let wrong = ()
  > [%%endif]
  > EOF
  $ cat > expected-nested-structure-items.ml <<EOF
  > let before = "kept"
  > let after = "kept"
  > EOF
  $ ./dump_pps.exe nested-structure-items.ml > actual
  $ sed '/^$/d' actual > actual.normalized
  $ sed '/^$/d' expected-nested-structure-items.ml > expected.normalized
  $ diff -u expected.normalized actual.normalized

Malformed [%%if] blocks report stable failures.

  $ cat > bad-operator.ml <<EOF
  > [%%if ocaml_version ~~ (1, 2, 3)]
  > let unreachable = ()
  > [%%endif]
  > EOF
  $ ./dump_pps.exe bad-operator.ml
  Failure: invalid [%%if] header
  [1]

  $ cat > duplicate-else.ml <<EOF
  > [%%if ocaml_version < (999, 0, 0)]
  > let branch = "then"
  > [%%else]
  > let branch = "else"
  > [%%else]
  > let branch = "duplicate"
  > [%%endif]
  > EOF
  $ ./dump_pps.exe duplicate-else.ml
  Failure: unexpected [%%else]
  [1]

  $ cat > unterminated.ml <<EOF
  > [%%if ocaml_version < (999, 0, 0)]
  > let branch = "then"
  > EOF
  $ ./dump_pps.exe unterminated.ml
  Failure: unterminated [%%if]
  [1]
