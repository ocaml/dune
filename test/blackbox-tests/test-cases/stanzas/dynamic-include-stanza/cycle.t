
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
     dynamic_include b/dune in directory a
  -> dynamic_include a/dune in directory b
  -> dynamic_include b/dune in directory a
  -> required by alias default
  [1]
