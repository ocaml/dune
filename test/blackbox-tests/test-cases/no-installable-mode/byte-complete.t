Byte_complete is allowed to be installable since 3.6

  $ pkg=foobarbaz
  $ bin=testbin
  $ make_dune_project 3.5
  $ touch $pkg.opam testbin.ml
  $ cat >dune <<EOF
  > (executable
  >  (name $bin)
  >  (public_name $bin)
  >  (modes byte_complete))
  > EOF
  $ dune build @install
  File "dune", line 4, characters 8-21:
  4 |  (modes byte_complete))
              ^^^^^^^^^^^^^
  Error: byte_complete is only available since version 3.6 of the dune
  language. Please update your dune-project file to have (lang dune 3.6).
  [1]
  $ make_dune_project 3.6
  $ dune build $pkg.install
  $ grep $bin _build/default/$pkg.install
    "_build/install/default/bin/testbin"
