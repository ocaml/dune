  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using ctypes 0.3)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (ctypes
  >   (external_library_name fooBar)
  >   (build_flags_resolver vendored)
  >   (generated_entry_point Types_generated2)
  >   (type_description
  >    (instance Type)
  >    (functor Type_description))))
  > EOF

  $ bash -c 'set -o pipefail; dune build 2>&1 | head -n 20'
  File "dune", lines 1-9, characters 0-211:
  1 | (library
  2 |  (name foo)
  3 |  (ctypes
  4 |   (external_library_name fooBar)
  5 |   (build_flags_resolver vendored)
  6 |   (generated_entry_point Types_generated2)
  7 |   (type_description
  8 |    (instance Type)
  9 |    (functor Type_description))))
  Error: Module Type_description is required by ctypes at dune:9 but is missing
  in the modules field of the stanza.
  [1]
