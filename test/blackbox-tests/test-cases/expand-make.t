On Unix, dune should lookup gmake and followed by make.

Regression test for https://github.com/ocaml/dune/issues/5470 

  $ mkdir make gmake

  $ cat >make/make <<EOF
  > #!/usr/bin/env sh
  > echo make
  > EOF

  $ cat >gmake/gmake <<EOF
  > #!/usr/bin/env sh
  > echo gmake
  > EOF

  $ chmod +x make/make gmake/gmake

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias make)
  >  (action (run %{make})))
  > EOF

  $ PATH="$PWD/make:$PATH" dune build @make --force
  make
  $ PATH="$PWD/gmake:$PWD/make:$PATH" dune build @make --force
  gmake
