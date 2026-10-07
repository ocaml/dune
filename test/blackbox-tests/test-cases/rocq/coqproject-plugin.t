A generated _RocqProject must make workspace-local plugins discoverable without
relying on Dune's process environment.

  $ make_rocq_project 3.24 0.14
  $ cat >> dune-project <<'EOF'
  > (package
  >  (name test-plugin))
  > EOF

  $ mkdir plugin theory
  $ cat > plugin/dune <<'EOF'
  > (library
  >  (name test_plugin)
  >  (public_name test-plugin.plugin))
  > EOF
  $ cat > plugin/test_plugin.ml <<'EOF'
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

  $ dune build @rocqproject theory/Test.vo

The arguments in the generated _RocqProject files are sufficient to compile the file in a clean environment.

  $ arguments=$(sed -re 's,-arg ,,' _build/default/theory/_RocqProject | tr '\n' " ")
  $ (cd theory && env -u OCAMLPATH rocq compile -q $arguments Test.v) 2>/dev/null
  [1]

Importing a compiled theory file that declares the plugin also works with the same arguments

  $ (cd theory && env -u OCAMLPATH rocq compile -q $arguments RequireTest.v) 2>/dev/null
  [1]
