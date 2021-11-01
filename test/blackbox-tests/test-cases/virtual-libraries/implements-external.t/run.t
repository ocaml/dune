Test that we can implement external libraries.

First we create an external library
  $ dune build --root vlib @install
  Entering directory 'vlib'

Then we make sure that we can implement it
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl --debug-dependency-path
  Entering directory 'impl'
  bar from vlib
  Foo.run implemented

Make sure that we can also implement native only variants
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-native-only --debug-dependency-path
  Entering directory 'impl-native-only'
  implement virtual module

We can implement external variants with mli only modules
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-intf-only --debug-dependency-path
  Entering directory 'impl-intf-only'
  implemented mli only
  magic number: 42

Implement external virtual libraries with private modules
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-private-module --debug-dependency-path
  Entering directory 'impl-private-module'
  Name: implement virtual module. Magic number: 42
