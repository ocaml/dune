Test that section pforms are substituted with absolute paths.

  $ . ./helpers.sh

  $ add_mock_repo_if_needed

  $ mkpkg test <<EOF
  > install: [
  >  ["echo" "--prefix" prefix]
  >  ["echo" "--prefix=%{prefix}%"]
  > ]
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
  --prefix $SANDBOX/_private/default/.pkg/test/target
  $SANDBOX/_private/default/.pkg/test/target
