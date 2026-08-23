Malformed source META file templates are rejected at their source location
instead of being installed.

  $ make_dune_project_with_package 2.7 foobarlib

  $ cat >foobarlib.ml <<EOF
  > let foo () = ()
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foobarlib))
  > EOF

  $ cat >META.foobarlib.template <<EOF
  > package "broken" (
  > # DUNE_GEN
  > EOF

  $ dune build @install
  File "META.foobarlib.template", line 3, characters 0-0:
  Error: Invalid META template for package foobarlib.
  1 closing parentheses missing
  [1]
