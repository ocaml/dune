Testing `dune ocaml top` with absolute path arguments. Reproduces #15109
(sub-issue of #12230).

  $ make_dune_project 3.25

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name foolib))
  > EOF
  $ cat > lib/foolib.ml <<EOF
  > let hw () = print_endline "foolib"
  > EOF

A relative path works and emits directives for the library:

  $ dune ocaml top lib | grep -c "#load"
  1

CR-someday Alizter: an absolute path pointing at the same directory should
work identically to the relative form. Today it crashes with a Code_error
because Path.Build.relative is called with an absolute string.

  $ dune ocaml top $PWD/lib 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]

CR-someday Alizter: an absolute path that is genuinely outside the
workspace should produce a clean user error rather than a crash.

  $ dune ocaml top /tmp/does-not-exist 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]
