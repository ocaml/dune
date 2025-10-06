Test that the parameters support access to the dependencies

  $ . ./helpers.sh
  $ init_project

Creates a library that exports a module we can later import as part of the library

  $ make_dir_with_dune "signature" <<EOF
  > (library
  >  (name signature))
  > EOF
  $ cat > "signature/signature.ml" <<EOF
  > module type S = sig
  >  val some_int: int
  > end
  > let do_something (module X : S) = print_int X.some_int
  > EOF

Use the library in a library_parameter.

  $ make_dir_with_dune "param_intf" <<EOF
  > (library_parameter
  >  (name param_intf)
  >  (libraries signature))
  > EOF
  $ cat > "param_intf/param_intf.mli" <<EOF
  > module M : Signature.S
  > EOF

We build the expected `cmi` and check if it is available from the _build dir.
This test will be extended later to support the implementation and the
parametrized library.

  $ dune build $(target_cmi "param_intf")

We ensure it built the parameter.

  $ ocamlobjinfo "$(build_target_cmi 'param_intf')" | grep "Is parameter"
  Is parameter: YES

A library parameterized by this parameter has transitive access to the `signature`
library by default:

  $ make_dir_with_dune "mylib" <<EOF
  > (library
  >   (name mylib)
  >   (parameters param_intf))
  > EOF
  $ cat > mylib/mylib.ml <<EOF
  > let test () = Signature.do_something (module Param_intf.M)
  > EOF

  $ dune build

Unless the dune-project disables transitive dependencies:

  $ cat >> dune-project <<EOF
  > (implicit_transitive_deps false)
  > EOF

  $ dune build
  File "mylib/mylib.ml", line 1, characters 14-36:
  1 | let test () = Signature.do_something (module Param_intf.M)
                    ^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module Signature
  [1]

In which case the parameter should explicitly re-export `signature`:

  $ cat > param_intf/dune <<EOF
  > (library_parameter
  >  (name param_intf)
  >  (libraries (re_export signature)))
  > EOF

  $ dune build

  $ ocamlobjinfo "$(build_target_cmi 'mylib')" | grep -o Signature
  Signature
