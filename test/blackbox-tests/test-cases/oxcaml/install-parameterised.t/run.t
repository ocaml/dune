Test that an external definition of parameters and parameterised libraries can be used.

First we "install" the external library:

  $ dune build --root external @install
  Entering directory 'external'
  Leaving directory 'external'

Then we test the installation:

  $ mkdir user
  $ cd user
  $ export OCAMLPATH=../external/_build/install/default/lib

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > EOF

We test that a library can be parameterised by an external `library_parameter`
definition, and can implement an external parameter too:

  $ mkdir other_impl
  $ echo 'let v = "other(" ^ Param.v ^ ")"' > other_impl/other_impl.ml
  $ cat > other_impl/dune <<EOF
  > (library (name other_impl)
  >   (parameters external.param)
  >   (implements external.param))
  > EOF

We test that a library can instantiate with an external argument implementing
the external parameter:

  $ mkdir other_ext
  $ echo 'let v = "compose(" ^ Other_impl.v ^ ")"' > other_ext/other_ext.ml
  $ cat > other_ext/dune <<EOF
  > (library (name other_ext)
  >   (implements external.param)
  >   (libraries (instantiate other_impl external.impl)))
  > EOF

We test that a binary can instantiate external libraries, with either external
and local implementations:

  $ mkdir bin
  $ cat > bin/bin.ml <<EOF
  > let () =
  >   print_endline Paramlib_impl.v ;
  >   print_endline Paramlib_otherext.v ;
  >   print_endline (Lib.test ()) ;
  >   print_endline Unwrap_lib.(Unwrapped_a.a ^ "," ^ Unwrapped_b.b) ;
  >   print_endline (Rewrap.Unwrapped_a.a ^ "," ^ Rewrap.Unwrapped_b.b) ;
  >   print_endline Other_ext.v
  > EOF
  $ cat > bin/dune <<EOF
  > (executable (name bin)
  >   (libraries
  >     (instantiate external.paramlib external.impl :as paramlib_impl)
  >     (instantiate external.paramlib other_ext :as paramlib_otherext)
  >     external.lib ; has instances internally
  >     (instantiate external.unwrapped_lib external.impl :as unwrap_lib)
  >     (instantiate external.unwrapped_lib other_ext :as rewrap)
  >     other_ext))
  > EOF

  $ dune exec bin/bin.exe
  paramlib(helper(external.impl))
  paramlib(helper(compose(other(external.impl))))
  PARAMLIB(HELPER(EXTERNAL.IMPL)) PARAMLIB(HELPER(EXTERNAL.IMPL2))
  unwrapped_a:external.impl,unwrapped_b:external.impl
  unwrapped_a:compose(other(external.impl)),unwrapped_b:compose(other(external.impl))
  compose(other(external.impl))
