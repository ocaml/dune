Test unused library detection with external libraries

The unused-libs alias should detect unused libraries, including external ones.
Currently, external libraries are not properly handled.

Setup external libraries in findlib

  $ mkdir -p findlib/ext_used findlib/ext_unused

Create META files for the external libraries:

  $ cat > findlib/ext_used/META <<EOF
  > description = "External library that will be used"
  > version = "1.0"
  > archive(byte) = "ext_used.cma"
  > archive(native) = "ext_used.cmxa"
  > EOF

  $ cat > findlib/ext_unused/META <<EOF
  > description = "External library that will NOT be used"
  > version = "1.0"
  > archive(byte) = "ext_unused.cma"
  > archive(native) = "ext_unused.cmxa"
  > EOF

Create minimal compiled files for the external libraries:

  $ cat > findlib/ext_used/ext_used.ml <<EOF
  > let used_function x = x + 1
  > EOF

  $ cat > findlib/ext_unused/ext_unused.ml <<EOF
  > let unused_function x = x * 2
  > EOF

Compile the external libraries:

  $ (cd findlib/ext_used && ocamlc -a -o ext_used.cma ext_used.ml)
  $ (cd findlib/ext_unused && ocamlc -a -o ext_unused.cma ext_unused.ml)

Create a dune project that uses these external libraries:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (libraries ext_used ext_unused))
  > EOF

  $ cat > mylib.ml <<EOF
  > (* Only use ext_used, not ext_unused *)
  > let compute x = Ext_used.used_function x
  > EOF

Build the unused-libs alias:

  $ OCAMLPATH=$PWD/findlib dune build @unused-libs
  File "dune", line 3, characters 21-31:
  3 |  (libraries ext_used ext_unused))
                           ^^^^^^^^^^
  Error: Unused libraries:
  - ext_unused
  [1]

The ext_unused library is correctly detected as unused since mylib.ml only
imports Ext_used, not Ext_unused.
