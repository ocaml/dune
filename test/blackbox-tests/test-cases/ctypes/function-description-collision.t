We create a buildable with a ctypes field that declares a duplicate
(function_description). This should display a nice error message instead of a
crash.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using ctypes 0.3)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name e)
  >  (ctypes
  >   (external_library_name ext)
  >   (build_flags_resolver (vendored))
  >   (type_description
  >    (functor T)
  >    (instance T))
  >   (function_description
  >    (functor F)
  >    (instance FI))
  >   (function_description
  >    (functor F)
  >    (instance FI))
  >   (generated_entry_point Entry)))
  > EOF

  $ touch e.ml

  $ cat > t.ml << EOF
  > module Types (_:Ctypes.TYPE) = struct
  > end
  > EOF

  $ cat > f.ml << EOF
  > module Functions (_:Ctypes.FOREIGN) = struct
  > end
  > EOF

  $ dune build
  File "dune", lines 12-14, characters 2-56:
  12 |   (function_description
  13 |    (functor F)
  14 |    (instance FI))
  Error: Only a single (function_description) can instantiate F as FI.
  [1]
