Multiple %{bin:...} deps in a single rule.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name foo) (package mypkg) (modules foo))
  > (executable (public_name bar) (package mypkg) (modules bar))
  > EOF
  $ cat >src/foo.ml <<'EOF'
  > let () = print_endline "hello from foo"
  > EOF
  $ cat >src/bar.ml <<'EOF'
  > let () = print_endline "hello from bar"
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps %{bin:foo} %{bin:bar})
  >  (action
  >   (with-stdout-to path-output
  >    (bash "echo $PATH"))))
  > EOF

Both binaries live in the same install bin dir, which is on PATH:

  $ dune build path-output
  $ env_added "$(cat _build/default/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/install/default/bin

The rule depends on both staging binaries:

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths'
  "_build/install/default/bin/bar"
  "_build/install/default/bin/foo"

Both binaries are callable by name:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps %{bin:foo} %{bin:bar})
  >  (action
  >   (with-stdout-to run-output
  >    (bash "foo && bar"))))
  > EOF
  $ dune build run-output
  $ cat _build/default/run-output
  hello from foo
  hello from bar
