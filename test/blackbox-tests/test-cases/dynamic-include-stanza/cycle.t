
  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ mkdir a b

  $ cat >a/dune<<EOF
  > (dynamic_include ../b/dune)
  > EOF

  $ cat >b/dune<<EOF
  > (dynamic_include ../a/dune)
  > EOF

  $ dune build
  Error: Dependency cycle between:
     dynamic_include b/dune in directroy a
  -> dynamic_include a/dune in directroy b
  -> dynamic_include b/dune in directroy a
  -> required by alias default
  [1]
