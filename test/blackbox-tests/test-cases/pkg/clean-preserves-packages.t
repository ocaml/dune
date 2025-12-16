Test that `dune clean` preserves package management data by default,
and `dune clean --full` removes everything.

  $ . ./helpers.sh

Create a simple package dependency:

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name bar) (depends foo) (allow_empty))
  > EOF

Build the package:

  $ build_pkg foo

Verify the package directory exists:

  $ test -d _build/_private/default/.pkg && echo "Package dir exists"
  Package dir exists

Now run `dune clean` (without --full):

  $ dune clean

The .pkg directory should still exist:

  $ test -d _build/_private/default/.pkg && echo "Package dir preserved"
  Package dir preserved

The package target (with cookie) should still exist:

  $ test -f "$(get_build_pkg_dir foo)/target/cookie" && echo "Package cookie preserved"
  Package cookie preserved

Now test that `dune clean --full` removes everything:

  $ dune clean --full

The _build directory should be gone:

  $ test -d _build && echo "_build exists" || echo "_build removed"
  _build removed

Rebuild and verify everything works:

  $ build_pkg foo

  $ test -d _build/_private/default/.pkg && echo "Package rebuilt"
  Package rebuilt
