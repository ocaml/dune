Remove write permissions from a sandbox directory and observe the error we get

  $ make_directory_targets_project 3.11

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (action (bash "mkdir foo && touch foo/bar && chmod -w foo")))
  > EOF

  $ dune build ./foo --sandbox=copy 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  Error:
  rename(_build/.sandbox/$SANDBOX/default/foo): Permission denied
  -> required by _build/default/foo
  [1]

Manual cleaning step so the dune executing the test suite doesn't croak trying
to delete the readonly dir
  $ chmod -R +w _build && rm -rf _build
