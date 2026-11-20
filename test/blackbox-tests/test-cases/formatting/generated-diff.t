Generated corrections can be formatted as part of the diff action.

The project setting requires Dune 3.26:

  $ mkdir old-project
  $ cd old-project
  $ make_dune_project 3.25
  $ echo '(format_generated_corrections true)' >> dune-project
  $ dune build
  File "dune-project", line 2, characters 0-35:
  2 | (format_generated_corrections true)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'format_generated_corrections' is only available since version 3.26 of
  the dune language. Please update your dune-project file to have (lang dune
  3.26).
  [1]
  $ cd ..

With no correction to compare, an optional diff succeeds even if the dialect
formatter refers to a missing input:

  $ mkdir custom
  $ cd custom
  $ make_dune_project 3.26
  $ cat >> dune-project <<'EOF'
  > (formatting (enabled_for custom))
  > (dialect
  >  (name custom)
  >  (implementation
  >   (extension custom)
  >   (format (cat %{input-file} suffix))))
  > EOF
  $ echo 'unformatted' > source.custom
  $ cat > dune <<'EOF'
  > (rule
  >  (alias missing-correction)
  >  (action (diff? source.custom source.custom.generated)))
  > EOF
  $ dune build @missing-correction
  $ cd ..

OCamlFormat receives the original source name and formats the correction in
place:

  $ mkdir ocaml
  $ cd ocaml
  $ make_dune_project 3.26
  $ echo '(format_generated_corrections true)' >> dune-project
  $ echo 'let original = 0' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to source.ml.generated
  >     (echo "let   generated = 1"))
  >    (diff? source.ml source.ml.generated))))
  > EOF
  $ dune build @generate --auto-promote 2>&1 | grep 'fake ocamlformat is running'
  fake ocamlformat is running: "--impl" "--inplace" "--name" "source.ml" "source.ml.generated"
  [1]
  $ cat source.ml
  (* fake ocamlformat output *)

Correction formatting is off by default, even with Dune 3.26:

  $ cd ..
  $ mkdir default
  $ cd default
  $ make_dune_project 3.26
  $ echo 'let original = 0' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to source.ml.generated
  >     (echo "let   generated = 1"))
  >    (diff? source.ml source.ml.generated))))
  > EOF
  $ dune build @generate --auto-promote >/dev/null 2>&1
  [1]
  $ cat source.ml
  let   generated = 1

Inline-test corrections are formatted before being offered for promotion:

  $ cd ..
  $ mkdir inline-tests
  $ cd inline-tests
  $ make_dune_project 3.26
  $ echo '(format_generated_corrections true)' >> dune-project
  $ cat > dune <<'EOF'
  > (library
  >  (name test)
  >  (inline_tests)
  >  (preprocess (pps ppx_expect)))
  > EOF
  $ cat > test.ml <<'EOF'
  > let%expect_test _ = print_endline "generated"; [%expect {| |}]
  > EOF
  $ dune build @runtest --auto-promote 2>&1 | grep 'fake ocamlformat is running'
  fake ocamlformat is running: "--impl" "--inplace" "--name" "test.ml" "test.ml.corrected"
  [1]
  $ cat test.ml
  (* fake ocamlformat output *)

Generated dune files use Dune's built-in formatter. The correction is consumed
by [diff?] rather than exposed as an intermediate build target:

  $ cd ..
  $ mkdir dune-file
  $ cd dune-file
  $ make_dune_project 3.26
  $ echo '(format_generated_corrections true)' >> dune-project
  $ echo '(rule  (alias generated))' > generated
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (copy generated dune.generated)
  >    (diff? dune dune.generated))))
  > EOF
  $ dune build dune.generated
  Error: Don't know how to build dune.generated
  [1]
  $ dune build @generate --auto-promote >/dev/null 2>&1
  [1]
  $ cat dune
  (rule
   (alias generated))

A directory correction can replace a source file without running a file formatter:

  $ cd ..
  $ mkdir directory-replacement
  $ cd directory-replacement
  $ make_dune_project 3.26
  $ echo '(format_generated_corrections true)' >> dune-project
  $ echo 'original' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (system "mkdir correction; echo generated > correction/contents")
  >    (diff? source.ml correction))))
  > EOF
  $ dune build @generate --auto-promote > output 2>&1
  [1]
  $ grep 'fake ocamlformat' output
  [1]
  $ test -d source.ml && cat source.ml/contents
  generated
