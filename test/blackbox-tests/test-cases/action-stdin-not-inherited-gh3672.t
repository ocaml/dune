Tests that actions do not inherit stdin by default.

  $ make_dune_project 2.7
  $ cat >dune <<EOF
  > (rule (with-stdout-to file.txt (run cat)))
  > EOF
  $ echo foo | dune build
  $ cat _build/default/file.txt
