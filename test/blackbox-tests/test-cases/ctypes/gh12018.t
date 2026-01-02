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
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune exec ./foo.exe 2>&1 | head -3
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("link_many: unable to find module",
