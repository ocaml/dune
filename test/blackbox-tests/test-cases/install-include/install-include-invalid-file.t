Including a file in the install stanza which does not contain a sexp list

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name hello))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name hello))
  > 
  > (install
  >  (files (include foo.sexp))
  >  (section share))
  > EOF

  $ cat >hello.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

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
