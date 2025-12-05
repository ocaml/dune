Test that errors with opam package manifests are only printed a single time
even when they are encountered by multiple concurrent runs of the opam solver.

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Project depending on a package "foo":
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Create the package "foo" with an invalid opam file:
  $ mkpkg foo <<EOF
  > invalid opam file
  > EOF

  $ dune pkg lock
  File "$TESTCASE_ROOT/mock-opam-repository/packages/foo/foo.0.0.1/opam", line 2, characters 8-12:
  2 | invalid opam file
              ^^^^
  Error: unable to parse opam file
  Parse error
  [1]

Create the package "foo" with an opam file that creates a circular dep with the project:
  $ mkpkg foo <<EOF
  > depends: [ "x" ]
  > EOF

  $ dune pkg lock
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "foo" is not in the workspace but it
  depends on the package "x" which is in the workspace.
  [1]

Create the package "foo" with an invalid variable interpolation:
  $ mkpkg foo <<EOF
  > build: [ "./configure" "--prefix=%{prefix" ]
  > EOF

  $ dune pkg lock
  File "$TESTCASE_ROOT/mock-opam-repository/packages/foo/foo.0.0.1/opam", line 1, characters 0-0:
  Error: Encountered malformed variable interpolation while processing commands
  for package foo.0.0.1.
  The variable interpolation:
  %{prefix
  [1]

Revert foo to a valid package:
  $ mkpkg foo

Update dune-package to pin the dune package:
  $ mkdir dune-to-pin
  $ cat > dune-to-pin/dune-project <<EOF
  > (lang dune 3.20)
  > (package (name dune))
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (pin
  >  (url "file://$PWD/dune-to-pin")
  >  (package (name dune)))
  > (package
  >  (name x)
  >  (depends foo))
  > EOF
  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  File "dune-project", line 4, characters 1-22:
  4 |  (package (name dune)))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: Dune cannot be pinned. The currently running version is the only one
  that may be used
  [1]

Revert dune-project to be valid:
  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

Add a file to the package but change its permission to not be readable:
  $ pkg_dir=$mock_packages/foo/foo.0.0.1
  $ mkdir -p $pkg_dir/files
  $ touch $pkg_dir/files/foo.txt
Make sure to undo the permission change on exit.
  $ trap "chmod +r $pkg_dir/files" EXIT
  $ chmod -r $pkg_dir/files
  $ dune pkg lock
  Warning: Unable to read directory
  mock-opam-repository/packages/foo/foo.0.0.1/files. Ignoring.
  Remove this message by ignoring by adding:
  (dirs \ files)
  to the dune file: mock-opam-repository/packages/foo/foo.0.0.1/dune
  Reason: opendir(mock-opam-repository/packages/foo/foo.0.0.1/files): Permission denied
  File "$TESTCASE_ROOT/mock-opam-repository/packages/foo/foo.0.0.1/files", line 1, characters 0-0:
  Error: Unable to read file in opam repository:
  opendir($TESTCASE_ROOT/mock-opam-repository/packages/foo/foo.0.0.1/files/): Permission denied
  [1]
