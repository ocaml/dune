Test that installed binaries with .exe extension are visible in the workspace
on Windows. This is a regression test for
https://github.com/ocaml/dune/issues/12433

On Windows, built executables have a .exe extension. The .install file lists
them with .exe, but lookups use bare names (e.g. "foo" not "foo.exe"). This
test verifies that the binary map correctly handles the .exe extension.

  $ make_lockdir

Create a package that installs a .exe binary. We pre-create the binary and
.install file in the package's .files directory:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (run bash -c "\| cp /usr/bin/echo.exe foo.exe;
  >               "\| cat >test.install <<INSTALL
  >               "\| bin: [ "foo.exe" ]
  >               "\| INSTALL
  >  ))
  > EOF

The binary should be visible in the workspace via %{bin:foo}:

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout
  >   (run %{bin:foo} workspace-test)))
  > EOF

  $ dune build ./testout && cat _build/default/testout
  File "dune", line 3, characters 7-17:
  3 |   (run %{bin:foo} workspace-test)))
             ^^^^^^^^^^
  Error: Program foo not found in the tree or in PATH
   (context: default)
  [1]
