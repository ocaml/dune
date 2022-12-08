Dune should not allow mod.ml and mod/ in the same directory:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > module X = Mod
  > EOF
  $ touch mod.ml
  $ mkdir mod
  $ touch mod/baz.ml

  $ dune build foo.exe

Another type of overlap:

  $ rm mod/baz.ml
  $ touch mod/mod.ml

  $ dune build foo.exe
