Testing the instantiation of parameterised inline tests.

  $ cat >> dune-project <<EOF
  > (lang dune 3.20)
  > (using oxcaml 0.1)
  > EOF

We first define a parameter signature:

  $ mkdir param
  $ echo 'val param : string' > param/param.mli
  $ cat > param/dune <<EOF
  > (library_parameter (name param))
  > EOF

Then a parameterised library, which uses inline tests:

  $ mkdir lib
  $ cat > lib/lib.ml <<EOF
  > let param = Param.param
  > let%test _ = Param.param = "impl"
  > EOF
  $ cat > lib/dune <<EOF
  > (library
  >   (name lib)
  >   (parameters param)
  >   (inline_tests)
  >   (preprocess (pps ppx_inline_test)))
  > EOF

Running the test fails, because we did not specify an implementation for the
parameter:

  $ dune runtest
  File "lib/dune", line 3, characters 14-19:
  3 |   (parameters param)
                    ^^^^^
  Error: To run the inline tests, please provide the missing parameter "param".
  -> required by
     _build/default/lib/.lib.inline-tests/.t.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/lib/.lib.inline-tests/inline-test-runner.exe
  -> required by _build/default/lib/.lib.inline-tests/partitions-best
  -> required by alias lib/runtest-lib in lib/dune:4
  -> required by alias lib/runtest in lib/dune:1
  Hint: Add (arguments ...) to the inline_tests to specify which implementation
  of the parameter "param" to use.
  [1]

We add an implementation:

  $ mkdir impl
  $ echo 'let param = "impl"' > impl/impl.ml
  $ cat > impl/dune <<EOF
  > (library
  >   (name impl)
  >   (implements param))
  > EOF

And specify that `(inline_tests)` should use it with `(arguments impl)`:

  $ cat > lib/dune <<EOF
  > (library
  >   (name lib)
  >   (parameters param)
  >   (inline_tests (arguments impl))
  >   (preprocess (pps ppx_inline_test)))
  > EOF

It should work:

  $ dune runtest

We break the test to confirm that the inline test is running:

  $ cat > lib/lib.ml <<EOF
  > let param = "lib(" ^ Param.param ^ ")"
  > let%test _ = Param.param = "not impl"
  > EOF

  $ dune runtest
  File "lib/lib.ml", line 2, characters 0-37: <<Param.param = "not impl">> is false.
  
  FAILED 1 / 1 tests
  [1]

Using another implementation:

  $ mkdir not_impl
  $ echo 'let param = "not impl"' > not_impl/not_impl.ml
  $ cat > not_impl/dune <<EOF
  > (library
  >   (name not_impl)
  >   (implements param))
  > EOF

  $ cat > lib/dune <<EOF
  > (library
  >   (name lib)
  >   (parameters param)
  >   (inline_tests (arguments not_impl))
  >   (preprocess (pps ppx_inline_test)))
  > EOF

This now works:

  $ dune runtest

Adding another library which has a dependency on the parameterised `lib`:

  $ mkdir lib2
  $ cat > lib2/lib2_util.ml <<EOF
  > let lib_param = Lib.param
  > EOF
  $ cat > lib2/lib2.ml <<EOF
  > let%test _ = Lib2_util.lib_param = "lib(impl)"
  > EOF
  $ cat > lib2/dune <<EOF
  > (library
  >   (name lib2)
  >   (parameters param)
  >   (libraries lib)
  >   (inline_tests (arguments impl))
  >   (preprocess (pps ppx_inline_test)))
  > EOF

(Note that the library has two files, which triggers the inline_test
preprocessor to generate `.pp.ml` files, which influences how the parameterised
libraries can read the ocamldep outputs since the filenames are not the
unpreprocessed ones.)

This should also work:

  $ dune runtest

Using the wrong implementation should break the test again:

  $ cat > lib2/dune <<EOF
  > (library
  >   (name lib2)
  >   (parameters param)
  >   (libraries lib)
  >   (inline_tests (arguments not_impl))
  >   (preprocess (pps ppx_inline_test)))
  > EOF

  $ dune runtest
  File "lib2/lib2.ml", line 1, characters 0-46: <<Lib2_util.lib_param = "lib(impl)">> is false.
  
  FAILED 1 / 1 tests
  [1]
