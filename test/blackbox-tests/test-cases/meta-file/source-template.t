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

The malformed source template reaches both generated and installed output.

  $ grep '^package "broken" (' _build/default/META.foobarlib
  package "broken" (
  $ grep '^package "broken" (' _build/install/default/lib/foobarlib/META
  package "broken" (
