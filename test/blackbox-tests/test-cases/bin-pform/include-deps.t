%{bin:...} inside (include ...) deps is processed: the binary's
.binaries dir gets added to the action's PATH and the binary is a
tracked dep of the rule.

  $ make_mypkg_bin_project

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

The action's PATH includes the .binaries dir:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST

The rule depends on the build artifact and the .binaries symlink:

  $ dune rules --format=json _build/default/path-output \
  >   | jq_dune '.[] | ruleDepFilePaths' \
  >   | grep mybin | censor
  "_build/default/src/mybin.bc"
  "_build/install/default/.binaries/$DIGEST/mybin"
