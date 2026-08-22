Promoted META file templates are validated against the generated contents even
when a stale source counterpart exists.

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
  File "_build/default/META.promoted.template", line 1, characters 17-18:
  1 | package "broken" @
                       ^
  Error: Invalid META template for package promoted.
  invalid character
  [1]
