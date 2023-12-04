Globs with absolute paths result in an error

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

Put $PWD in a file that can be read with the %{read:...} pform, so that the underline
in the error message is of consisntent length on different systems.
  $ printf $PWD > pwd

  $ touch foo.txt bar.txt

Absolute paths work in non-recursive globs, but are not permitted in the install stanza.
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files %{read:pwd}/*.txt))
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 20-37:
  2 |  (files (glob_files %{read:pwd}/*.txt))
                          ^^^^^^^^^^^^^^^^^
  Error: Absolute paths are not allowed in the install stanza.
  [1]

Absolute paths are not supported in recursive globs.
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec %{read:pwd}/*.txt))
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 24-41:
  2 |  (files (glob_files_rec %{read:pwd}/*.txt))
                              ^^^^^^^^^^^^^^^^^
  Error: Absolute paths in recursive globs are not supported.
  [1]

