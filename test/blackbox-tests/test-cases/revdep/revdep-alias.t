Check that the @revdep alias builds reverse dependencies of libraries in a directory.

  $ make_dune_project 3.20

  $ mkdir -p a b c d e

  $ cat >a/dune <<'EOF'
  > (library (name a))
  > (rule
  >  (alias default)
  >  (targets built)
  >  (action (with-stdout-to %{targets} (echo A))))
  > EOF

  $ cat >b/dune <<'EOF'
  > (library (name b) (libraries a))
  > (rule
  >  (alias default)
  >  (targets built)
  >  (action (with-stdout-to %{targets} (echo B))))
  > EOF
  $ cat >b/b.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >c/dune <<'EOF'
  > (library (name c) (libraries b))
  > (rule
  >  (alias default)
  >  (targets built)
  >  (action (with-stdout-to %{targets} (echo C))))
  > EOF
  $ cat >c/c.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >d/dune <<'EOF'
  > (library (name d))
  > (rule
  >  (alias default)
  >  (targets built)
  >  (action (with-stdout-to %{targets} (echo D))))
  > EOF
  $ cat >d/d.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >e/dune <<'EOF'
  > (executable (name e) (libraries a))
  > (rule
  >  (alias default)
  >  (targets built)
  >  (action (with-stdout-to %{targets} (echo E))))
  > EOF
  $ cat >e/e.ml <<'EOF'
  > let () = ()
  > EOF

  $ dune build @a/revdep
  $ cat _build/default/b/built
  B
  $ test -e _build/default/c/built && echo "should not build c" || echo "c not built"
  c not built
  $ cat _build/default/e/built
  E

  $ dune build @b/revdep
  $ cat _build/default/c/built
  C

  $ dune build @c/revdep

  $ dune build @d/revdep
