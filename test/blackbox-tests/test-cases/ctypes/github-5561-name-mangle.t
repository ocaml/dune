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
  Error: Module Type_description is required by ctypes at dune:9 but is missing
  in the modules field of the stanza.
  [1]
