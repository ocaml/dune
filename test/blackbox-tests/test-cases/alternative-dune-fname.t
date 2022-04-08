Check support for alternative dune file name "dune-file".

The feature is not supported in < 3.0.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (accept_alternative_dune_file_name)
  > EOF
  $ dune build
  File "dune-project", line 2, characters 0-35:
  2 | (accept_alternative_dune_file_name)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'accept_alternative_dune_file_name' is only available since version
  3.0 of the dune language. Please update your dune-project file to have (lang
  dune 3.0).
  [1]

The feature *is* supported in 3.0 and later, but it is opt-in:

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (rule (alias foo) (action (echo "In dune")))
  > EOF

  $ cat >dune-file <<EOF
  > (rule (alias foo) (action (echo "In dune-file")))
  > EOF

  $ dune build @foo
  In dune

Need to enable explicitly:

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (accept_alternative_dune_file_name)
  > EOF

  $ dune build @foo
  In dune-file
