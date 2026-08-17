Malformed source META file templates are currently installed without a
diagnostic. Snapshot that behavior before adding validation.

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
