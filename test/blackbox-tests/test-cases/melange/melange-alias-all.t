Melange compilation is added to `@all`

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using melange 1.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mlib)
  >  (modes melange))
  > EOF
  $ cat > hello.ml <<EOF
  > let x = "hello"
  > EOF

  $ dune build @all --display=short
  $ find _build/default | grep '\.cm'
  [1]
