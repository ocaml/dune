Test that we can implement external libraries.

First we create an external library
  $ dune build --root vlib @install
  Entering directory 'vlib'
  Leaving directory 'vlib'

Then we make sure that we can implement it
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build @default @install --root impl
  Entering directory 'impl'
  bar from vlib
  Foo.run implemented
  Leaving directory 'impl'

Make sure that we can also implement native only variants
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-native-only --debug-dependency-path
  Entering directory 'impl-native-only'
  implement virtual module
  Leaving directory 'impl-native-only'

We can implement external variants with mli only modules
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-intf-only --debug-dependency-path
  Entering directory 'impl-intf-only'
  implemented mli only
  magic number: 42
  Leaving directory 'impl-intf-only'

Implement external virtual libraries with private modules
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl-private-module --debug-dependency-path
  Entering directory 'impl-private-module'
  Name: implement virtual module. Magic number: 42
  Leaving directory 'impl-private-module'

Now we test the following use case:
- A virtual library and its implementation are installed
- We are able to use the implementation as an external dependency in a project
Currently, dune's behavior is broken in this situation. The virtual library's
modules remain hidden.
  $ export OCAMLPATH=$PWD/vlib/_build/install/default/lib:$PWD/impl/_build/install/default/lib
  $ mkdir use-external-impl && cd use-external-impl
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name blah)
  >  (libraries dune-vlib.impl))
  > EOF
  $ cat >blah.ml <<EOF
  > Vlib.Foo.run ()
  > EOF
  $ dune exec ./blah.exe
  bar from vlib
  Foo.run implemented
  $ cd ..
