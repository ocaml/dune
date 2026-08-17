Testing `dune describe pp` with absolute path arguments. Reproduces
#15093 (sub-issue of #12230). The fixture uses a real preprocessor
(sed substitution) so the test exercises the pp pipeline, which is
what `dune describe pp` is meant to inspect.

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (preprocess (action (run sed -e s/REPLACE/preprocessed/ %{input-file}))))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "REPLACE"
  > EOF

A relative path works:

  $ dune describe pp foo.ml
  let () = print_endline "preprocessed"

CR-someday Alizter: an absolute path pointing at the same file should work
identically to the relative form. Today it crashes with a Code_error.

  $ dune describe pp $PWD/foo.ml 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]

An absolute path that is genuinely outside the workspace should produce a
clean user error rather than a crash.

  $ dune describe pp /tmp/does-not-exist.ml 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]
