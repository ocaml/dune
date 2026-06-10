Testing `dune utop` with absolute path arguments. Reproduces #15108
(sub-issue of #12230).

  $ make_dune_project 3.25

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name foolib))
  > EOF
  $ cat > lib/foolib.ml <<EOF
  > let hw () = print_endline "foolib"
  > EOF

  $ cat > init.ml <<EOF
  > Foolib.hw ()
  > EOF

A relative path works:

  $ dune utop lib -- init.ml
  foolib

CR-someday Alizter: an absolute path pointing at the same directory should
work identically to the relative form. Today it crashes with a Code_error
because Path.Build.relative is called with an absolute string.

  $ dune utop $PWD/lib -- init.ml 2>&1 | grep "Internal error"
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]

An absolute path that is genuinely outside the workspace should produce a
clean user error rather than a crash.

  $ dune utop /tmp/does-not-exist 2>&1 | grep "cannot find"
  Error: cannot find directory: /tmp/does-not-exist
  [1]
