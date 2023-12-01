When a ctypes description has just a type description, if should be possible to
build the library.
See #9300.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using ctypes 0.3)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name l)
  >  (ctypes
  >   (external_library_name none)
  >   (build_flags_resolver vendored)
  >   (type_description
  >    (functor bindings)
  >    (instance types))
  >   (generated_entry_point c)))
  > EOF

  $ cat > bindings.ml << EOF
  > module Types(F:Ctypes.TYPE) = struct end
  > EOF

  $ dune build
