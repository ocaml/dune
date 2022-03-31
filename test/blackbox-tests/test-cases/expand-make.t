On Unix, dune should lookup gmake and followed by make.

Regression test for https://github.com/ocaml/dune/issues/5470 

This needs special setup to have a PATH which does not contain a gmake binary,
in case the system PATH has one. Instead of prepending to PATH we create a bin/
subdirectory with links to all the executables dune needs.

  $ mkdir make gmake bin

  $ for prog in dune ocamlc sh; do
  >   ln -s $(which $prog) bin/;
  > done

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
  > (data_only_dirs bin)
  > (rule
  >  (alias make)
  >  (action (run %{make})))
  > EOF

  $ PATH="$PWD/make:$PWD/bin" dune build @make --force;
  make
  $ PATH="$PWD/gmake:$PWD/make:$PWD/bin" dune build @make --force
  gmake
