Dune files names dune-file should be formatted

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (accept_alternative_dune_file_name)
  > EOF

  $ cat >dune-file <<EOF
  > (rule
  > (with-stdout-to foo (echo bar)))
  > EOF

  $ dune fmt

  $ cat dune-file
  (rule
  (with-stdout-to foo (echo bar)))
