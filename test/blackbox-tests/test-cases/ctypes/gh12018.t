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
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("link_many: unable to find module",
     { main_module_name = "Libexample__type_gen"
     ; modules =
         Modules
           (Singleton
              { source =
                  { path = [ "Foo" ]
                  ; files =
                      { impl =
                          Some
                            { path = In_build_dir "default/foo.ml"
                            ; original_path = In_build_dir "default/foo.ml"
                            ; dialect = "ocaml"
                            }
                      ; intf =
                          Some
                            { path = In_build_dir "default/foo.mli"
                            ; original_path = In_build_dir "default/foo.mli"
                            ; dialect = "ocaml"
                            }
                      }
                  }
              ; obj_name = "dune__exe__Foo"
              ; pp = None
              ; visibility = "public"
              ; kind = "impl"
              ; install_as = None
              })
     })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_rules__Exe.link_many.(fun) in file "src/dune_rules/exe.ml",
    lines 312-316, characters 12-15
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune/src/exn.ml", line 38, characters 27-56
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default")
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
