Ensure reverse deps include executables and tests, not just libraries.

  $ make_dune_project 3.20

  $ mkdir core dep testapp

  $ cat >core/dune <<'EOF'
  > (library (name corelib))
  > (rule (alias default) (targets core.built) (action (with-stdout-to %{targets} (echo CORE))))
  > EOF
  $ cat >core/corelib.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >dep/dune <<'EOF'
  > (executable (name tool) (modules tool) (libraries corelib))
  > (rule (alias default) (targets tool.built) (action (with-stdout-to %{targets} (echo TOOL))))
  > EOF
  $ cat >dep/tool.ml <<'EOF'
  > let () = ()
  > EOF

  $ cat >testapp/dune <<'EOF'
  > (tests
  >  (names t1)
  >  (libraries corelib))
  > (rule (alias default) (targets test.built) (action (with-stdout-to %{targets} (echo TEST))))
  > EOF
  $ cat >testapp/t1.ml <<'EOF'
  > let () = ()
  > EOF

  $ dune build @core/revdep
  $ cat _build/default/dep/tool.built
  TOOL
  $ cat _build/default/testapp/test.built
  TEST
