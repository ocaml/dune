This test ensures the `implements` field is working in the context of
parameterized libraries.

  $ . ./helpers.sh

We create two parameters, on public, the other one local.

  $ init_project

  $ make_dir_with_dune "foo" <<EOF
  > (library_parameter
  >   (public_name foo))
  > EOF
  $ make_dummy_intf "foo" "foo"
  $ cat >> dune-project << EOF
  > (package (name foo))
  > EOF

  $ make_dir_with_dune "bar" <<EOF
  > (library_parameter
  >   (name bar))
  > EOF
  $ make_dummy_intf "bar" "bar"

  $ dune build $(target_cmi "bar")
  $ dune build $(target_cmi "foo")

  $ make_dir_with_dune "foo_lib" <<EOF
  > (library
  >  (name foo_lib))
  > EOF
  $ cat > "foo_lib/foo_lib.ml" <<EOF
  > let x = 42
  > EOF
  $ dune build


We implements a non parameter library (neither a virtual module). It should
fail with the correct error message.

  $ rm -rf _build
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements missing_foo))
  > EOF
  $ dune build
  File "foo_impl/dune", line 3, characters 13-24:
  3 |  (implements missing_foo))
                   ^^^^^^^^^^^
  Error: Library "missing_foo" not found.
  -> required by alias default
  [1]

We implements the parameter using library calling a wrong parameter name.

  $ rm -rf _build foo_impl
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements missing_foo))
  > EOF
  $ dune build
  File "foo_impl/dune", line 3, characters 13-24:
  3 |  (implements missing_foo))
                   ^^^^^^^^^^^
  Error: Library "missing_foo" not found.
  -> required by alias default
  [1]

We implement the parameter using a library with a correct parameter this time.

  $ rm -rf _build foo_impl
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF
  $ make_dummy_impl "foo_impl" "foo_impl"
  $ dune build

We change the implementation to be public instead of a local one.

  $ cat >> dune-project << EOF
  > (package (name foo_impl))
  > EOF
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (public_name foo_impl)
  >  (implements foo))
  > EOF
  $ dune build

We create library implementing the parameter with a bigger interface than what
the parameter expects.

  $ rm -rf _build
  $ echo "let ignore_me = 42" >> foo_impl/foo_impl.ml

  $ dune build

We add a library implementing a parameter with the wrong interface.

  $ rm -rf _build
  $ echo "type t = int" > foo_impl/foo_impl.ml

  $ dune build
  File "foo_impl/foo_impl.ml", line 1:
  Error: The argument module foo_impl/foo_impl.ml
         does not match the parameter signature foo_impl/.foo_impl.objs/byte/foo.cmi:
          The value f is required but not provided
         File "foo/foo.mli", line 2, characters 0-17: Expected declaration
  [1]

A library implementing the parameter, but importing the content from other files.

  $ rm -rf _build

  $ echo "type t = int" > foo_impl/aux_type.ml
  $ echo "type t" > foo_impl/aux_type.mli

  $ echo "let f _ = ()" > foo_impl/aux_impl.ml
  $ echo "val f: Aux_type.t -> unit" > foo_impl/aux_impl.mli

  $ cat > foo_impl/foo_impl.ml <<EOF
  > include Aux_type
  > include Aux_impl
  > EOF
  $ dune build

We ensure we have all the necessary information for the impletamentation to be used with findlib.

  $ dune build @install
  $ cat _build/install/default/lib/foo_impl/dune-package | grep "implements"
   (implements foo)
