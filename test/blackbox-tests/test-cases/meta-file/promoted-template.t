A promoted META file template may have a source counterpart that differs from
the rule output. The generated contents are currently accepted.

  $ make_dune_project_with_package 2.7 promoted

  $ cat >promoted.ml <<EOF
  > let foo () = ()
  > EOF

  $ cat >META.promoted.template <<EOF
  > # DUNE_GEN
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (public_name promoted))
  > 
  > (rule
  >  (target META.promoted.template)
  >  (mode promote)
  >  (action
  >   (write-file %{target} "package \"broken\" @")))
  > EOF

  $ dune build @install --disable-promotion

The source remains valid, while the generated template and final META contain
the malformed rule output.

  $ printf '# DUNE_GEN\n' | cmp - META.promoted.template
  $ printf 'package "broken" @' | cmp - _build/default/META.promoted.template
  $ printf 'package "broken" @' | cmp - _build/default/META.promoted
