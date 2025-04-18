Test that dune can solve dependencies with opam repos using a layout other than
the one used by the official opam repo. Opam allows arbitrary dependency
hierarchies inside the "package" directory.

Make a custom opam repo:
  $ mkdir -p custom-opam-repo/packages
  $ cat > custom-opam-repo/repo <<EOF
  > opam-version: "2.0"
  > EOF

Make some packgaes at various depths in the directory hierarchy:
  $ mkdir -p custom-opam-repo/packages/a.1
  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/a.1/opam

  $ mkdir -p custom-opam-repo/packages/b/b.2
  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/b/b.2/opam

  $ mkdir -p custom-opam-repo/packages/foo/bar/baz/c.3
  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/foo/bar/baz/c.3/opam

Set up the workspace to use only the custom repo:
  $ cat > dune-workspace <<EOF
  > (lang dune 3.18)
  > (repository
  >  (name custom)
  >  (url file://$PWD/custom-opam-repo))
  > (lock_dir
  >  (repositories custom))
  > EOF

Project to depend on some packages from the custom repo:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (allow_empty)
  >  (name x)
  >  (depends a b c))
  > EOF

Test that dune can solve the project:
  $ dune pkg lock
  Solution for dune.lock:
  - a.1
  - b.2
  - c.3


Now test that dune can handle some degenerate cases.

Make a copy of the c.3 package at a different location within the repo:

  $ mkdir -p custom-opam-repo/packages/foo/bar/baz/qux/c.3
  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/foo/bar/baz/qux/c.3/opam

  $ dune pkg lock 2>&1 | sed -n '/^Warning:/,$p'
  Warning: Multiple occurrences of package c.3 in repo
  "$TESTCASE_ROOT/custom-opam-repo":
   - packages/foo/bar/baz/qux/c.3/opam
   - packages/foo/bar/baz/c.3/opam
  
  
  Dune will use the metadata in packages/foo/bar/baz/qux/c.3/opam
  Solution for dune.lock:
  - a.1
  - b.2
  - c.3

  $ rm -r custom-opam-repo/packages/foo/bar/baz/qux/c.3

Create an opam file in a directory that doesn't have a version number. There's
no way to refer to this package in dune-project but dune should still be able
to parse the repo with crashing.

  $ mkdir -p custom-opam-repo/packages/foo/
  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/foo/opam

  $ dune pkg lock 2>&1 | sed -n '/^Warning:/,$p'
  Warning: The repo
  "$TESTCASE_ROOT/custom-opam-repo"
  contains an opam file at location "packages/foo" which cannot be interpreted
  as an opam package. Opam files must be contained in directories named like
  <name>.<version>.
  Solution for dune.lock:
  - a.1
  - b.2
  - c.3

  $ rm custom-opam-repo/packages/foo/opam

Put an opam file directly inside the "packages" directory:

  $ echo 'opam-version: "2.0"' > custom-opam-repo/packages/opam

  $ dune pkg lock 2>&1 | sed -n '/^Warning:/,$p'
  Warning: The repo
  "$TESTCASE_ROOT/custom-opam-repo"
  contains an opam file at location "packages" which cannot be interpreted as
  an opam package. Opam files must be contained in directories named like
  <name>.<version>.
  Solution for dune.lock:
  - a.1
  - b.2
  - c.3

  $ rm custom-opam-repo/packages/opam
