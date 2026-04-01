Skip unavailable reverse dependencies.

  $ make_dune_project 3.20

  $ mkdir a good bad

  $ cat >a/dune <<'EOF'
  > (library (name a))
  > EOF

  $ cat >good/dune <<'EOF'
  > (library (name good) (libraries a))
  > (rule (alias default) (targets good.built) (action (with-stdout-to %{targets} (echo GOOD))))
  > EOF
  $ cat >good/good.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >bad/dune <<'EOF'
  > (library
  >  (name bad)
  >  (optional)
  >  (libraries a missinglib))
  > (rule (alias default) (targets bad.built) (action (with-stdout-to %{targets} (echo BAD))))
  > EOF
  $ cat >bad/bad.ml <<'EOF'
  > let x = ()
  > EOF

  $ dune build @a/revdep
  $ cat _build/default/good/good.built
  GOOD
  $ test -e _build/default/bad/bad.built && echo "should not build bad" || echo "bad not built"
  bad not built
