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
