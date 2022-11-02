  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (using ctypes 0.1)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (ctypes
  >   (external_library_name fooBar)
  >   (generated_entry_point Types_generated2)
  >   (type_description
  >    (instance Type)
  >    (functor Type_description))))
  > EOF

  $ dune build
  File "fooBar__type_gen.ml", line 3, characters 12-34:
  3 |     (module Type_description.Types)
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module Type_description
  File "dune", line 1, characters 0-177:
  1 | (library
  2 |  (name foo)
  3 |  (ctypes
  4 |   (external_library_name fooBar)
  5 |   (generated_entry_point Types_generated2)
  6 |   (type_description
  7 |    (instance Type)
  8 |    (functor Type_description))))
  Error: No rule found for libfoo_stubs.a
  File "dune", line 2, characters 7-10:
  2 |  (name foo)
             ^^^
  Package fooBar was not found in the pkg-config search path.
  Perhaps you should add the directory containing `fooBar.pc'
  to the PKG_CONFIG_PATH environment variable
  No package 'fooBar' found
  [1]
