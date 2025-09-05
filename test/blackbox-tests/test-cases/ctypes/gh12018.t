Reproduction case for https://github.com/ocaml/dune/issues/12018

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using ctypes 0.3)
  > EOF

  $ cat > type_description.ml <<EOF
  > module Types (F : Ctypes.TYPE) = struct end
  > EOF

  $ cat > function_description.ml <<EOF
  > open Ctypes
  > module Types = Types_generated
  >  
  > module Functions (F : Ctypes.FOREIGN) = struct
  >   open F
  >   let add2 =
  >     foreign "example_add2" (int @-> returning int)
  > end
  > EOF

  $ cat > foo.ml <<EOF
  > let () = Printf.printf "%d" (C.Functions.add2 2)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (ctypes
  >   (external_library_name libexample)
  >   (headers (include "example.h"))
  >   (build_flags_resolver pkg_config)
  >   (type_description
  >    (instance Types)
  >    (functor Type_description))
  >   (function_description
  >    (instance Functions)
  >    (functor Function_description))
  >   (generated_entry_point C)))
  > EOF

  $ LIBEX=$(realpath "$PWD/libexample")
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune exec ./foo.exe
  File "dune", line 3, characters 10-13:
  3 |  (modules foo)
                ^^^
  Error: Module Function_description is required by ctypes at dune:13 but is
  missing in the modules field of the stanza.
  File "dune", line 3, characters 10-13:
  3 |  (modules foo)
                ^^^
  Error: Module Type_description is required by ctypes at dune:10 but is
  missing in the modules field of the stanza.
  [1]

