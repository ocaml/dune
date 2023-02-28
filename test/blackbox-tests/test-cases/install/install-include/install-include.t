Simple example of including a file in the install stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > (package (name hello))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name hello))
  > 
  > (install
  >  (files a.txt (include foo.sexp))
  >  (section share))
  > EOF

  $ cat >hello.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ touch a.txt

  $ dune build @install
  File "dune", line 5, characters 14-32:
  5 |  (files a.txt (include foo.sexp))
                    ^^^^^^^^^^^^^^^^^^
  Error: 'include' is only available since version 3.5 of the dune language.
  Please update your dune-project file to have (lang dune 3.5).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name hello))
  > EOF

  $ dune build @install
  File "_unknown_", line 1, characters 0-0:
  Error: No rule found for foo.sexp
  [1]

  $ cat >foo.sexp <<EOF
  > (b.txt c.txt)
  > EOF

  $ touch b.txt

  $ dune build @install
  File "_build/default/foo.sexp", line 1, characters 7-12:
  1 | (b.txt c.txt)
             ^^^^^
  Error: No rule found for c.txt
  [1]

  $ touch c.txt

  $ dune build @install

  $ cat _build/default/hello.install
  lib: [
    "_build/install/default/lib/hello/META"
    "_build/install/default/lib/hello/dune-package"
  ]
  bin: [
    "_build/install/default/bin/hello"
  ]
  share: [
    "_build/install/default/share/hello/a.txt"
    "_build/install/default/share/hello/b.txt"
    "_build/install/default/share/hello/c.txt"
  ]
