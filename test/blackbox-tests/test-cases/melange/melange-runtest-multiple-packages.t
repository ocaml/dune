Show interaction of `dune runtest -p ..` and `(melange.emit ..)`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > (package (name a))
  > (package (name b))
  > EOF

  $ mkdir a b
  $ cat > a/dune <<EOF
  > (melange.emit
  >  (alias runtest)
  >  (package a)
  >  (emit_stdlib false)
  >  (target out))
  > EOF
  $ cat > a/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (alias runtest)
  >  (package b)
  >  (emit_stdlib false)
  >  (target out))
  > EOF
  $ cat > b/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF

Selecting only the package a should not build b

  $ dune runtest -p a
  $ test -e _build/default/b/out/b/x.js
  [1]

  $ dune runtest -p b
  $ test -e _build/default/b/out/b/x.js
