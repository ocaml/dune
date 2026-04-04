Tests for the `dune describe` command
====================================

The following tests verify that `dune describe workspace` returns
consistent dependencies for executables and libraries, in the
particular case of dependencies towards PPX-rewriters.

They witness an issue reported in #6486, that is fixed in #6727

Setup
-----

  $ make_dune_project 3.4
  $ mkdir static_lib
  $ cd static_lib
  $ cat >dune <<EOF
  > (library
  >  (name static_lib))
  > EOF
  $ touch static_lib.ml
  $ cd ..
  $ mkdir dummy_ppx
  $ cd dummy_ppx
  $ cat >dune <<EOF
  > (library
  >  (name dummy_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib static_lib))
  > EOF
  $ cat >dummy_ppx.ml <<EOF
  > (* dummy PPX rewriter, for use in tests *)
  > (* artificial static dependency on library static_lib *)
  > module M = Static_lib
  > let () =
  >   Ppxlib.Driver.register_transformation
  >     "dummy2"
  >     ~impl:(fun s -> s)
  > EOF
  $ cd ..
  $ mkdir lib
  $ cd lib
  $ cat >dune <<EOF
  > (library
  >  (name lib)
  >  (preprocess (pps dummy_ppx)))
  > EOF
  $ touch lib.ml
  $ cd ..
  $ mkdir exe
  $ cd exe
  $ cat >dune <<EOF
  > (executable
  >  (name exe)
  >  (preprocess (pps dummy_ppx)))
  > EOF
  $ touch exe.ml
  $ cd ..

Describe various things
-----------------------

Warning: when testing the ``dune describe workspace`` command, do not
forget to pass the ``--sanitize-for-tests`` flags, so that the tests
are reproducible, and are kept consistent between different machines.
``dune describe workspace`` may indeed print absolute paths, that are
not stable across different setups.

  $ dune describe workspace --lang 0.1 --sanitize-for-tests exe
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (exe))
     (requires ())
     (modules
      (((name Exe)
        (impl (_build/default/exe/exe.ml))
        (intf ())
        (cmt (_build/default/exe/.exe.eobjs/byte/dune__exe__Exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/exe/.exe.eobjs/byte)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests lib | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name lib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Lib)
        (impl (_build/default/lib/lib.ml))
        (intf ())
        (cmt (_build/default/lib/.lib.objs/byte/lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/lib/.lib.objs/byte)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests --with-pps exe | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (exe))
     (requires
      ($DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST))
     (modules
      (((name Exe)
        (impl (_build/default/exe/exe.ml))
        (intf ())
        (cmt (_build/default/exe/.exe.eobjs/byte/dune__exe__Exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/exe/.exe.eobjs/byte))))
   (library
    ((name compiler-libs)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name compiler-libs.common)
     (uid $DIGEST)
     (local false)
     (requires ($DIGEST))
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid $DIGEST)
     (local true)
     (requires
      ($DIGEST
       $DIGEST
       $DIGEST))
     (source_dir _build/default/dummy_ppx)
     (modules
      (((name Dummy_ppx)
        (impl (_build/default/dummy_ppx/dummy_ppx.ml))
        (intf ())
        (cmt (_build/default/dummy_ppx/.dummy_ppx.objs/byte/dummy_ppx.cmt))
        (cmti ()))))
     (include_dirs (_build/default/dummy_ppx/.dummy_ppx.objs/byte))))
   (library
    ((name ocaml-compiler-libs.common)
     (uid $DIGEST)
     (local false)
     (requires ($DIGEST))
     (source_dir /FINDLIB/ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB/ppx_derivers))))
   (library
    ((name ppxlib)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST))
     (source_dir /FINDLIB/ppxlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name static_lib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/static_lib)
     (modules
      (((name Static_lib)
        (impl (_build/default/static_lib/static_lib.ml))
        (intf ())
        (cmt (_build/default/static_lib/.static_lib.objs/byte/static_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/static_lib/.static_lib.objs/byte))))
   (library
    ((name stdlib-shims)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests --with-pps lib | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name compiler-libs)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name compiler-libs.common)
     (uid $DIGEST)
     (local false)
     (requires ($DIGEST))
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid $DIGEST)
     (local true)
     (requires
      ($DIGEST
       $DIGEST
       $DIGEST))
     (source_dir _build/default/dummy_ppx)
     (modules
      (((name Dummy_ppx)
        (impl (_build/default/dummy_ppx/dummy_ppx.ml))
        (intf ())
        (cmt (_build/default/dummy_ppx/.dummy_ppx.objs/byte/dummy_ppx.cmt))
        (cmti ()))))
     (include_dirs (_build/default/dummy_ppx/.dummy_ppx.objs/byte))))
   (library
    ((name lib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Lib)
        (impl (_build/default/lib/lib.ml))
        (intf ())
        (cmt (_build/default/lib/.lib.objs/byte/lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/lib/.lib.objs/byte))))
   (library
    ((name ocaml-compiler-libs.common)
     (uid $DIGEST)
     (local false)
     (requires ($DIGEST))
     (source_dir /FINDLIB/ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB/ppx_derivers))))
   (library
    ((name ppxlib)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST
       $DIGEST))
     (source_dir /FINDLIB/ppxlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid $DIGEST)
     (local false)
     (requires
      ($DIGEST $DIGEST))
     (source_dir /FINDLIB/ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name static_lib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/static_lib)
     (modules
      (((name Static_lib)
        (impl (_build/default/static_lib/static_lib.ml))
        (intf ())
        (cmt (_build/default/static_lib/.static_lib.objs/byte/static_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/static_lib/.static_lib.objs/byte))))
   (library
    ((name stdlib-shims)
     (uid $DIGEST)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims)))))
