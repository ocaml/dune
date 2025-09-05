Remove write permissions from a sandbox directory and observe the error we get

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (action (system "mkdir foo && touch foo/bar && chmod -w foo")))
  > EOF

  $ dune build ./foo --sandbox=copy 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  File "dune", lines 1-3, characters 0-90:
  1 | (rule
  2 |  (target (dir foo))
  3 |  (action (system "mkdir foo && touch foo/bar && chmod -w foo")))
  Error: failed to delete sandbox in
  _build/.sandbox/$SANDBOX
  Reason:
  rmdir(_build/.sandbox/$SANDBOX/default/foo): Directory not empty
  Error:
  rename(_build/.sandbox/$SANDBOX/default/foo): Permission denied
  -> required by _build/default/foo

Manual cleaning step so the dune executing the test suite doesn't croak trying
to delete the readonly dir
  $ chmod -R +w _build && rm -rf _build
