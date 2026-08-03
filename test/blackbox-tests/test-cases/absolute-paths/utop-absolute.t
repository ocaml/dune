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

An absolute path pointing at the same directory works identically to the
relative form:

  $ dune utop $PWD/lib -- init.ml
  foolib

An absolute path that is genuinely outside the workspace should produce a
clean user error rather than a crash.

  $ dune utop /tmp/does-not-exist 2>&1 | grep "cannot find"
  Error: cannot find directory: /tmp/does-not-exist
  [1]
