META file templates are validated after expansion. Failures introduced by
generated entries are attributed to the responsible generation marker.

  $ make_dune_project_with_package 2.7 rendered

  $ cat >rendered.ml <<EOF
  > let foo () = ()
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name rendered)
  >  (synopsis "generated marker"))
  > EOF

  $ cat >META.rendered.template <<'EOF'
  > # DUNE_GEN
  > package "broken"
  > # DUNE_GEN
  > (
  > )
  > EOF

  $ dune build @install
  File "META.rendered.template", line 3, characters 0-10:
  3 | # DUNE_GEN
      ^^^^^^^^^^
  Error: Invalid META template for package rendered.
  '(' expected
  [1]
