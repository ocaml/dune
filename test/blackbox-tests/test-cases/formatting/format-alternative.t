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
  File "dune-file", line 1, characters 0-0:
  Error: Files _build/default/dune-file and _build/default/.formatted/dune-file
  differ.
  Promoting _build/default/.formatted/dune-file to dune-file.
  [1]

  $ cat dune-file
  (rule
   (with-stdout-to
    foo
    (echo bar)))
