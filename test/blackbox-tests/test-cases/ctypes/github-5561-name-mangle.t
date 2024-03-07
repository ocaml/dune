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
  File "fooBar__type_gen.ml", line 3, characters 12-34:
  3 |     (module Type_description.Types)
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound module Type_description
  [1]
