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

PATH gets a bin-layout dir (with symlinks to both bins) plus the
install bin dir:

  $ dune build path-output
  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $PWD/_build/install/default/bin

The rule depends on each binary via two paths: the build artifact
(from the pform expansion) and the bin-layout symlink (from the PATH
machinery):

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' | censor
  "_build/default/src/bar.exe"
  "_build/default/src/foo.exe"
  "_build/install/default/.binaries/$DIGEST/bar"
  "_build/install/default/.binaries/$DIGEST/foo"

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
