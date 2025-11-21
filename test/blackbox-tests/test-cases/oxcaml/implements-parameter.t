This test ensures the `implements` field is working in the context of
parameterised libraries.

  $ . ./helpers.sh

We create a public parameter:

  $ init_project

  $ cat >> dune-project << EOF
  > (package (name public_foo))
  > EOF

  $ make_dir_with_dune "foo" <<EOF
  > (library_parameter
  >   (public_name public_foo)
  >   (name foo))
  > EOF
  $ make_dummy_intf "foo" "foo"

  $ dune build $(target_cmi "foo")

It should fail if we try to implement an unknown library:

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

It should also fail if we try to implement a library that is neither a
parameter nor virtual:

  $ make_dir_with_dune "a_standard_lib" <<EOF
  > (library
  >  (name a_standard_lib))
  > EOF

  $ rm -rf foo_impl
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements a_standard_lib))
  > EOF

  $ dune build
  File "foo_impl/dune", line 3, characters 13-27:
  3 |  (implements a_standard_lib))
                   ^^^^^^^^^^^^^^
  Error: Library "a_standard_lib" is neither a virtual library nor a library
  parameter. It cannot be implemented by "foo_impl".
  -> required by alias default
  [1]

We implement the parameter using a library with a correct parameter this time.

  $ rm -rf foo_impl
  $ make_dir_with_dune "foo_impl" <<EOF
  > (library
  >  (name foo_impl)
  >  (implements foo))
  > EOF
  $ make_dummy_impl "foo_impl" "foo_impl"
  $ dune build

The implementation can use multiple files, as long as the root module satisfies
the parameter interface:

  $ cat > foo_impl/foo_impl.ml <<EOF
  > type t = string
  > let f = Util.f
  > EOF
  $ cat > foo_impl/util.ml <<EOF
  > let f = print_endline
  > EOF

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

We create a library implementing the parameter with a bigger interface than what
the parameter expects.

  $ echo "let ignore_me = 42" >> foo_impl/foo_impl.ml

  $ dune build

The compiler will signal an error if the library doesn't implement the required
interface:

  $ echo "type t = int" > foo_impl/foo_impl.ml

  $ dune build
  File "foo_impl/foo_impl.ml", line 1:
  Error: The argument module foo_impl/foo_impl.ml
         does not match the parameter signature foo/.foo.objs/byte/foo.cmi: 
         The value f is required but not provided
         File "foo/foo.mli", line 2, characters 0-17: Expected declaration
  [1]

A library implementing the parameter, but importing the content from other files.

  $ echo "type t = int" > foo_impl/aux_type.ml
  $ echo "type t" > foo_impl/aux_type.mli

  $ echo "let f _ = ()" > foo_impl/aux_impl.ml
  $ echo "val f: Aux_type.t -> unit" > foo_impl/aux_impl.mli

  $ cat > foo_impl/foo_impl.ml <<EOF
  > include Aux_type
  > include Aux_impl
  > EOF
  $ dune build

We ensure we have all the necessary information for the implementation to be
used with findlib. It should use the public name of the parameter:

  $ dune build @install
  $ cat _build/install/default/lib/foo_impl/dune-package | grep "implements"
   (implements public_foo)

We introduce a new parameter that is not public:

  $ make_dir_with_dune "bar" <<EOF
  > (library_parameter
  >   (name bar))
  > EOF
  $ make_dummy_intf "bar" "bar"

A private library can implement this private parameter:

  $ make_dir_with_dune "bar_impl" <<EOF
  > (library
  >  (name bar_impl)
  >  (implements bar))
  > EOF
  $ make_dummy_impl "bar_impl" "bar_impl"
  $ dune build

We can check that the right flag was given to the compiler with ocamlobjinfo:

  $ ocamlobjinfo _build/default/bar_impl/bar_impl.cma | grep 'Parameter'
  Parameter implemented: Bar

A private library can also implement a public parameter:

  $ cat > bar_impl/dune <<EOF
  > (library
  >  (name bar_impl)
  >  (implements foo))
  > EOF
  $ dune build

  $ ocamlobjinfo _build/default/bar_impl/bar_impl.cma | grep Parameter
  Parameter implemented: Foo

But it's an error for a public library to implement a private parameter:

  $ cat >> dune-project << EOF
  > (package (name bar_impl))
  > EOF
  $ rm -rf bar_impl
  $ make_dir_with_dune "bar_impl" <<EOF
  > (library
  >  (public_name bar_impl)
  >  (implements bar))
  > EOF
  $ dune build
  File "bar_impl/dune", line 3, characters 13-16:
  3 |  (implements bar))
                   ^^^
  Error: Library "bar" is private, it cannot be a dependency of a public
  library. You need to give "bar" a public name.
  [1]

It's impossible for a library to implement two parameters:

  $ rm -rf bar_impl
  $ make_dir_with_dune "bar_impl" <<EOF
  > (library
  >  (name bar_impl)
  >  (implements foo bar))
  > EOF
  $ dune build
  File "bar_impl/dune", line 3, characters 17-20:
  3 |  (implements foo bar))
                       ^^^
  Error: Too many arguments for "implements"
  [1]

An unwrapped library can't implement a parameter:

  $ rm -rf bar_impl
  $ make_dir_with_dune "bar_impl" <<EOF
  > (library
  >  (name bar_impl)
  >  (wrapped false)
  >  (implements bar))
  > EOF
  $ dune build
  File "bar_impl/dune", line 3, characters 10-15:
  3 |  (wrapped false)
                ^^^^^
  Error: Wrapped cannot be set for implementations. It is inherited from the
  virtual library.
  [1]
