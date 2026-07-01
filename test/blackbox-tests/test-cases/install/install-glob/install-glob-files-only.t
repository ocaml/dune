Test that the `glob_files` terms are only accepted in the `files` field and not in
the `dirs` field

  $ make_dune_project_with_package 3.6 foo

  $ cat >dune <<EOF
  > (install
  >  (dirs (glob_files *))
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 7-21:
  2 |  (dirs (glob_files *))
             ^^^^^^^^^^^^^^
  Error: Invalid format, <name> or (<name> as <install-as>) expected
  [1]

  $ cat >dune <<EOF
  > (install
  >  (dirs (glob_files_rec *))
  >  (section share))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 7-25:
  2 |  (dirs (glob_files_rec *))
             ^^^^^^^^^^^^^^^^^^
  Error: Invalid format, <name> or (<name> as <install-as>) expected
  [1]
