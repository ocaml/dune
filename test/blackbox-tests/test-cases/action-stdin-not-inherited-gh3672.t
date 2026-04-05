  $ echo "(lang dune 2.7)" > dune-project
  $ cat >dune <<EOF
  > (rule (with-stdout-to file.txt (run cat)))
  > EOF
  $ echo foo | dune build
  $ cat _build/default/file.txt
