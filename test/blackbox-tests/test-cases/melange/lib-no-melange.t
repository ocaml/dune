Test cases building libraries with modules in upper case names

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (libraries foo)
  >  (module_system commonjs))
  > EOF

  $ cat > main.ml <<EOF
  > let t = Foo_Bar.t
  > EOF

  $ mkdir lib

  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false))
  > EOF
  $ cat > lib/Foo_Bar.ml <<EOF
  > let t = "Hello World"
  > EOF

  $ dune build output/melange__Main.js
  File "dune", line 3, characters 1-16:
  3 |  (libraries foo)
       ^^^^^^^^^^^^^^^
  Error: The library 'foo' was added as a dependency of the melange.emit stanza
  with target 'output', but this library is not compatible with melange. To fix
  this, either:
  - add (modes melange) to the library stanza
  - or remove the library from the libraries field in the melange.emit stanza
  [1]
