%{bin:...} inside (include ...) deps is processed: the binary's
bin-layout dir gets added to the action's PATH and the binary is a
tracked dep of the rule.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ cat >deps.sexp <<'EOF'
  > (%{bin:mybin})
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (include deps.sexp))
  >  (action
  >   (with-stdout-to path-output
  >    (bash "echo $PATH"))))
  > EOF

  $ dune build path-output

The action's PATH includes the bin-layout dir:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST

The rule depends on the build artifact and the bin-layout symlink:

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin | censor
  "_build/default/src/mybin.exe"
  "_build/install/default/.binaries/$DIGEST/mybin"
