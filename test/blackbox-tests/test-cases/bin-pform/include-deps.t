BUG: %{bin:...} inside (include ...) deps does not add the binary to
PATH. The bin_env is computed but discarded because Include calls
named_paths_builder recursively and cannot propagate the env upward.

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

The bin-layout dir should appear here (as it does for inline deps in
run-action.t) but it doesn't. Only the install bin dir (always-on)
shows:

  $ env_added "$(cat _build/default/path-output)" "$PATH"
  $TESTCASE_ROOT/_build/install/default/bin

The rule depends on the build artifact (via where=Original_path):

  $ dune rules --format=json _build/default/path-output \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/src/mybin.exe"
