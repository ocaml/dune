Include a file which contains the (foo as bar) syntax for renaming a file

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
  > ((a.txt as b/c.txt))
  > EOF

  $ touch a.txt

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
    "_build/install/default/share/hello/b/c.txt" {"b/c.txt"}
  ]
