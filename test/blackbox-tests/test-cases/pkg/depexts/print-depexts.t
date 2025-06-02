Show that the depexts that a project has can be printed.

  $ . ../helpers.sh
  $ mkrepo

Make a project:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > EOF

Create a lockdir with a package that features some depexts.

  $ make_lockdir
  $ cat > dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (depexts unzip gnupg)
  > EOF

Printing the depexts should show all the depexts that the project has:

  $ dune show depexts
  gnupg
  unzip
