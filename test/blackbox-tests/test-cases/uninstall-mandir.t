Test dune uninstallation of mangpages, and particularly the potential removal
of the manpages folders such as `<prefix>/man/man1/`, which may, or may not be
empty (because other tools/projects can install manpages to these directories,
either before, or after dune installs a project).

-------------------------------------------------------------------------------
Create a project that install a manpage & build it

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (name test)
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section  man)
  >  (files    test.1)
  >  (package  test)
  > )
  > EOF

  $ cat >test.1 <<EOF
  > Test
  > EOF

  $ touch test.opam

  $ dune build -p test @install

-------------------------------------------------------------------------------
Create a prefix

  $ mkdir -p install

-------------------------------------------------------------------------------
Install the project

  $ dune install --prefix=install -p test test --display short | dune_cmd sanitize
  Installing install/lib/test/META
  Installing install/lib/test/dune-package
  Installing install/lib/test/opam
  Installing install/man/man1/test.1

-------------------------------------------------------------------------------
Simulate another tool being installed and creating some manpages in the prefix

  $ mkdir -p install/man/man1 && touch install/man/man1/foobar

-------------------------------------------------------------------------------
Uninstall the project; since the manpage directory contains pages from other
tools, dune should not remove it, nor complain about it being non-empty.

  $ dune uninstall --prefix=install --display short -p test test | dune_cmd sanitize
  Deleting install/lib/test/META
  Deleting install/lib/test/dune-package
  Deleting install/lib/test/opam
  Deleting install/man/man1/test.1
  Error: Directory install/man/man1 is not empty, cannot delete (ignoring).
  Deleting empty directory install/lib/test

