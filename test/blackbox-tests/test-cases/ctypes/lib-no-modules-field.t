Checking the validation of ctypes with no (modules) field works.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using ctypes 0.3)
  > EOF

  $ mkdir bindings
  $ cat > bindings/dune <<EOF
  > (library (name mybindings))
  > EOF

  $ cat > bindings/ffi_bindings.ml <<EOF
  > module Types (F : Ctypes.TYPE) = struct end
  > module Functions (F : Ctypes.FOREIGN) = struct end
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name mylib)
  >  (libraries mybindings)
  >  (ctypes
  >   (external_library_name foo)
  >   (type_description (instance Types) (functor Ffi_bindings))
  >   (function_description (instance Functions) (functor Ffi_bindings))
  >   (generated_entry_point C)))
  > EOF

  $ dune build
  File "dune", lines 1-8, characters 0-245:
  1 | (library
  2 |  (name mylib)
  3 |  (libraries mybindings)
  4 |  (ctypes
  5 |   (external_library_name foo)
  6 |   (type_description (instance Types) (functor Ffi_bindings))
  7 |   (function_description (instance Functions) (functor Ffi_bindings))
  8 |   (generated_entry_point C)))
  Error: Module Ffi_bindings is required by ctypes at dune:6 but is missing in
  the modules field of the stanza.
  File "dune", lines 1-8, characters 0-245:
  1 | (library
  2 |  (name mylib)
  3 |  (libraries mybindings)
  4 |  (ctypes
  5 |   (external_library_name foo)
  6 |   (type_description (instance Types) (functor Ffi_bindings))
  7 |   (function_description (instance Functions) (functor Ffi_bindings))
  8 |   (generated_entry_point C)))
  Error: Module Ffi_bindings is required by ctypes at dune:7 but is missing in
  the modules field of the stanza.
  [1]
