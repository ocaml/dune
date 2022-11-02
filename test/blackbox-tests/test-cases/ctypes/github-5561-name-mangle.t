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

  $ dune build 2>&1 | head -n 2
  Internal error, please report upstream including the contents of _build/log.
  Description:
