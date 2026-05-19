%{bin:NAME} inside (include ...) deps is processed: the binary
becomes a tracked dep of the rule and the install bin dir is added
to the action's PATH.

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

The action's PATH includes the install bin dir:

  $ env_added "$(cat _build/default/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/install/default/bin

The rule depends on the staging binary:

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/install/default/bin/mybin"
