A META file template may parse before expansion but become invalid when generated
entries replace a later marker. This is currently accepted.

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

Both markers were expanded, and the malformed literal was installed between
them.

  $ grep -c '^description = "generated marker"$' _build/default/META.rendered
  2
  $ grep '^package "broken"$' _build/default/META.rendered
  package "broken"
  $ awk '/^package "broken"$/ { getline; print; exit }' _build/default/META.rendered
  description = "generated marker"
