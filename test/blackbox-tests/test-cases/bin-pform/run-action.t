%{bin:NAME} as a (run ...) target invokes the binary; as a deps
entry it adds a bin-layout dir to the action's PATH (plus the
always-on install bin dir).

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ cat >dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > (rule
  >  (deps %{bin:mybin})
  >  (action
  >   (progn
  >    (with-stdout-to path-output
  >     (bash "echo $PATH"))
  >    (run %{bin:mybin}))))
  > EOF
  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ dune build path-output
  hello from mybin
  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $PWD/_build/install/default/bin

The rule's deps include the build artifact and the bin-layout
symlink:

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin | censor
  "_build/default/mybin.exe"
  "_build/install/default/.binaries/$DIGEST/mybin"
