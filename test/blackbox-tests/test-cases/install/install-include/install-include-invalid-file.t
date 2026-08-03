Including a file in the install stanza which does not contain a sexp list

  $ make_install_include_project

  $ cat >foo.sexp <<EOF
  > a.txt
  > EOF

  $ dune build @install
  File "_build/default/foo.sexp", line 1, characters 0-5:
  1 | a.txt
      ^^^^^
  Error: Expected list, got:
  a.txt
  
  [1]
