When a ctypes description has just a type description, Dune does not setup a
rule for a stubs file it tries to use.
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
  File "dune", line 1, characters 0-185:
  1 | (library
  2 |  (name l)
  3 |  (ctypes
  4 |   (external_library_name none)
  5 |   (build_flags_resolver vendored)
  6 |   (type_description
  7 |    (functor bindings)
  8 |    (instance types))
  9 |   (generated_entry_point c)))
  Error: No rule found for libl_stubs.a
  [1]

Creating an empty file works around the issue.

  $ cat >> dune << EOF
  > (rule
  >  (write-file libl_stubs.a ""))
  > EOF

  $ dune build
