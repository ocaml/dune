Test that section pforms are substituted with absolute paths.

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (progn
  >  (run echo --prefix %{prefix})
  >  (run echo --prefix=%{prefix})))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends test))
  > EOF

Note that currently dune incorrectly substitutes relative paths for pforms that
appear in string interpolations.
  $ build_pkg test 2>&1 | strip_sandbox
  --prefix $SANDBOX/_private/default/.pkg/test.0.0.1-35943fe1ea902a1ac62aea8f115d162d/target
  $SANDBOX/_private/default/.pkg/test.0.0.1-35943fe1ea902a1ac62aea8f115d162d/target
