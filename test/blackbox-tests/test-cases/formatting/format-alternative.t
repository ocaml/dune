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
  --- dune-file
  +++ dune-file.corrected
  @@ -1,2 +1,4 @@
   (rule
  -(with-stdout-to foo (echo bar)))
  + (with-stdout-to
  +  foo
  +  (echo bar)))
  Promoting _build/default/dune-file.corrected to dune-file.

  $ cat dune-file
  (rule
   (with-stdout-to
    foo
    (echo bar)))
