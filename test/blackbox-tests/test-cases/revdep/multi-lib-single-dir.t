Check that multiple libraries in one directory contribute all reverse deps.

  $ make_dune_project 3.20

  $ mkdir src

  $ cat >src/dune <<'EOF'
  > (library (name a) (modules a))
  > (library (name a_helper) (modules a_helper))
  > (rule (alias default) (targets a.built) (action (with-stdout-to %{targets} (echo A))))
  > (rule (alias default) (targets helper.built) (action (with-stdout-to %{targets} (echo H))))
  > EOF
  $ cat >src/a.ml <<'EOF'
  > let x = ()
  > EOF
  $ cat >src/a_helper.ml <<'EOF'
  > let x = ()
  > EOF
  $ cat >src/a.ml <<'EOF'
  > let x = ()
  > EOF

  $ mkdir dep dep2

  $ cat >dep/dune <<'EOF'
  > (library (name dep) (libraries a))
  > (rule (alias default) (targets dep.built) (action (with-stdout-to %{targets} (echo DEP))))
  > EOF
  $ cat >dep/dep.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >dep2/dune <<'EOF'
  > (library (name dep2) (libraries a_helper))
  > (rule (alias default) (targets dep2.built) (action (with-stdout-to %{targets} (echo DEP2))))
  > EOF
  $ cat >dep2/dep2.ml <<'EOF'
  > let x = ()
  > EOF

  $ dune build @src/revdep
  $ cat _build/default/dep/dep.built
  DEP
  $ cat _build/default/dep2/dep2.built
  DEP2
