Tests package-scoped library dependencies.

  $ cat >dune-project <<EOF
  > (lang dune 1.0)
  > EOF

  $ cat >foo.opam

  $ cat >bar.opam

  $ cat >dune <<'EOF'
  > (library
  >  (name        foo)
  >  (public_name foo)
  >  (modules     foo))
  > 
  > (library
  >  (name        bar)
  >  (public_name bar)
  >  (libraries   foo)
  >  (modules     bar))
  > 
  > (rule
  >  (with-stdout-to foo.ml
  >   (echo "let x = 42")))
  > 
  > (rule
  >  (with-stdout-to bar.ml
  >   (echo "let x = string_of_int Foo.x")))
  > 
  > (rule
  >  (with-stdout-to test.ml
  >   (echo "let () = Printf.printf \"%d %s\" Foo.x Bar.x")))
  > 
  > (rule
  >  (deps    test.ml (package bar))
  >  (targets test.exe)
  >  (action  (run ocamlfind ocamlc -linkpkg -package bar -o test.exe test.ml)))
  > 
  > (alias
  >  (name   runtest)
  >  (action (run ./test.exe)))
  > EOF

  $ dune runtest
  42 42
