Begin by installing a library with C stubs.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package (name libA))
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name libA)
  >  (public_name libA)
  >  (foreign_stubs
  >   (language c)
  >   (names stubs)))
  > EOF
  $ cat >stubs.c <<EOF
  > int dummy() {
  >   return 0;
  > }
  > EOF
  $ dune build
  $ dune install --prefix ./install --display short
  Installing install/lib/libA/META
  Installing install/lib/libA/dune-package
  Installing install/lib/libA/libA.a
  Installing install/lib/libA/libA.cma
  Installing install/lib/libA/libA.cmi
  Installing install/lib/libA/libA.cmt
  Installing install/lib/libA/libA.cmx
  Installing install/lib/libA/libA.cmxa
  Installing install/lib/libA/libA.ml
  Installing install/lib/libA/liblibA_stubs.a
  Installing install/lib/libA/libA.cmxs
  Installing install/lib/stublibs/dlllibA_stubs.so
  $ cat ./install/lib/libA/dune-package
  (lang dune 3.16)
  (name libA)
  (sections
   (lib
    $TESTCASE_ROOT/install/lib/libA)
   (libexec
    $TESTCASE_ROOT/install/lib/libA)
   (stublibs
    $TESTCASE_ROOT/install/lib/stublibs))
  (files
   (lib
    (META
     dune-package
     libA.a
     libA.cma
     libA.cmi
     libA.cmt
     libA.cmx
     libA.cmxa
     libA.ml
     liblibA_stubs.a))
   (libexec (libA.cmxs))
   (stublibs (dlllibA_stubs.so)))
  (library
   (name libA)
   (kind normal)
   (archives (byte libA.cma) (native libA.cmxa))
   (plugins (byte libA.cma) (native libA.cmxs))
   (foreign_objects stubs.o)
   (foreign_archives (archives (for all) (files liblibA_stubs.a)))
   (foreign_dll_files ../stublibs/dlllibA_stubs.so)
   (native_archives libA.a)
   (main_module_name LibA)
   (modes byte native)
   (modules
    (wrapped
     (group
      (alias
       (obj_name libA)
       (visibility public)
       (kind alias)
       (source (path LibA) (impl (path libA.ml-gen))))
      (name LibA))
     (wrapped true))))

Now let us define an executable using that installed library.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name exeA)
  >  (libraries libA)
  >  (modes byte))
  > EOF
  $ touch exeA.ml
  $ OCAMLPATH=./install/lib dune build
