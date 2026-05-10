%{bin:NAME} as a (run ...) target invokes the binary; as a deps
entry it tracks the staging binary. The install bin dir is on the
action's PATH (always-on for actions, not added by the dep).

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
  $ env_added "$(cat _build/default/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/install/default/bin

Both pform usages dedupe to a single dep on the install staging
path:

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/install/default/bin/mybin"
