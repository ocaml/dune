A generated _RocqProject must make workspace-local plugins discoverable without
relying on Dune's process environment.

  $ cat >> dune-workspace <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cp dune-workspace dune-project

  $ mkdir dep dep/src
  $ cat >> dep/dune-project <<'EOF'
  > (lang dune 3.24)
  > (package
  >  (name dep-package))
  > EOF
  $ touch dep/dep-package.opam
  $ cat > dep/src/dune <<'EOF'
  > (library
  >  (name dep)
  >  (public_name dep-package.dep))
  > EOF
  $ touch dep/src/main.ml

  $ mkdir lib
  $ cd lib
  $ make_rocq_project 3.24 0.14
  $ cat >> dune-project <<'EOF'
  > (package
  >  (name test-plugin))
  > EOF

  $ mkdir plugin theory
  $ cat > plugin/dune <<'EOF'
  > (library
  >  (name test_plugin)
  >  (libraries dep-package.dep)
  >  (public_name test-plugin.plugin))
  > EOF
  $ cat > plugin/test_plugin.ml <<'EOF'
  > open Dep.Main
  > let () = ()
  > EOF

  $ cat > theory/dune <<'EOF'
  > (rocq.theory
  >  (name Test)
  >  (package test-plugin)
  >  (plugins test-plugin.plugin)
  >  (generate_project_file))
  > EOF
  $ cat > theory/Test.v <<'EOF'
  > Declare ML Module "test-plugin.plugin".
  > EOF
  $ cat > theory/RequireTest.v <<'EOF'
  > Require Import Test.Test.
  > EOF

  $ cd .. # back to the root

  $ cat > fake_repl.sh <<'EOF'
  > #!/bin/bash
  > printf '%s\n' "$@"
  > EOF
  $ chmod +x fake_repl.sh

The arguments computed for `dune rocq top` are sufficient to compile the file.

  $ (dune rocq top --toplevel $(pwd)/fake_repl.sh lib/theory/Test.v) > args
  $ (rocq top $(cat args) < lib/theory/Test.v) 2>/dev/null 1>/dev/null

  $ dune build

The arguments in the generated _RocqProject files are sufficient to compile the file.

  $ arguments=$(sed -re 's,-arg ,,' _build/default/lib/theory/_RocqProject | tr '\n' " ")
  $ (cd lib/theory && rocq compile -boot -noinit -q $arguments -o $(mktemp -d)/Test.vo Test.v) # 2>/dev/null

Importing a compiled theory file that declares the plugin also works with the same arguments

  $ (cd lib/theory && rocq compile -q $arguments RequireTest.v) 2>/dev/null
