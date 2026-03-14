Check that revdep-check/runtest/install build the matching aliases of reverse dependencies.

  $ make_dune_project 3.20

  $ mkdir a b exe

  $ cat >a/dune <<'EOF'
  > (library (name a))
  > EOF

  $ cat >b/dune <<'EOF'
  > (library (name b) (libraries a))
  > (rule (alias check) (targets b.check) (action (with-stdout-to %{targets} (echo CHECKB))))
  > (rule (alias runtest) (targets b.runtest) (action (with-stdout-to %{targets} (echo RUNB))))
  > (rule (alias install) (targets b.install) (action (with-stdout-to %{targets} (echo INSTB))))
  > EOF
  $ cat >b/b.ml <<'EOF'
  > let x = ()
  > EOF

  $ cat >exe/dune <<'EOF'
  > (executable (name tool) (libraries a))
  > (rule (alias check) (targets tool.check) (action (with-stdout-to %{targets} (echo CHECKTOOL))))
  > (rule (alias runtest) (targets tool.runtest) (action (with-stdout-to %{targets} (echo RUNTOOL))))
  > (rule (alias install) (targets tool.install) (action (with-stdout-to %{targets} (echo INSTTOOL))))
  > EOF
  $ cat >exe/tool.ml <<'EOF'
  > let () = ()
  > EOF

  $ dune build @a/revdep-check
  $ cat _build/default/b/b.check
  CHECKB
  $ cat _build/default/exe/tool.check
  CHECKTOOL

  $ dune build @a/revdep-runtest
  $ cat _build/default/b/b.runtest
  RUNB
  $ cat _build/default/exe/tool.runtest
  RUNTOOL

  $ dune build @a/revdep-install
  $ cat _build/default/b/b.install
  INSTB
  $ cat _build/default/exe/tool.install
  INSTTOOL
