When creating single module libraries & executables, running ocamldep isn't
necessary.

  $ make_dune_project 2.8
  $ cat >dune <<EOF
  > (executable (name foo))
  > EOF
  $ cat >foo.ml <<EOF
  > print_endline "hello world"
  > EOF
  $ dune exec ./foo.exe
  hello world

We check to see if ocamldep artifacts have been created:
  $ find _build/default -name "*.all-deps" -or -name "*.d"
