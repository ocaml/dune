Tests for the `dune describe` command
====================================

The following tests verify that `dune describe workspace` returns
consistent dependencies for executables and libraries, in the
particular case of dependencies towards PPX-rewriters.

They witness an issue reported in #6486, that is fixed in #6727

Setup
-----

  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > EOF
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

  $ dune describe workspace --lang 0.1 --sanitize-for-tests lib
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name lib)
     (uid 48f89f472eed42cfe8a7ae83e2c8c1ce)
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

  $ dune describe workspace --lang 0.1 --sanitize-for-tests --with-pps exe
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (exe))
     (requires
      (25fa301af563256248c5dadd60078c6e
       b3f1696f67e77afbbf90bbd4ff4db3f7
       3238f110cc121b8de5fb94811b5395f0
       aa150dea272f13137145f9b1336e6f89
       dc50e9309dbe56e9ba5a145dd0c7d272
       c1f4a4a8fcca6a7045ebee5b639dc729
       5df1715b4499126654748a00e0142c5e
       9087f2ff68e06ebafea537cd921b2b75
       3e0806da37ceb5ee0d783c15826db0e1
       b6a38388b521e9d788bf8c241c8b7131
       e7b78a2ef3334358a41a86855c8739c2
       1513262a352b79a04ff483cd84ebd966
       2c25d00eb24f552a85fba122b2d45dd1
       12c434f04237f2f66660cf4ef9d28bac
       d4bbc5a64c5463ce6eb84f54a61aa8b8))
     (modules
      (((name Exe)
        (impl (_build/default/exe/exe.ml))
        (intf ())
        (cmt (_build/default/exe/.exe.eobjs/byte/dune__exe__Exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/exe/.exe.eobjs/byte))))
   (library
    ((name compiler-libs)
     (uid 25fa301af563256248c5dadd60078c6e)
     (local false)
     (requires ())
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name compiler-libs.common)
     (uid b3f1696f67e77afbbf90bbd4ff4db3f7)
     (local false)
     (requires (25fa301af563256248c5dadd60078c6e))
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid d4bbc5a64c5463ce6eb84f54a61aa8b8)
     (local true)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729
       2c25d00eb24f552a85fba122b2d45dd1
       12c434f04237f2f66660cf4ef9d28bac))
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
     (uid 3238f110cc121b8de5fb94811b5395f0)
     (local false)
     (requires (b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid 5df1715b4499126654748a00e0142c5e)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid 3e0806da37ceb5ee0d783c15826db0e1)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB/ppx_derivers))))
   (library
    ((name ppxlib)
     (uid 2c25d00eb24f552a85fba122b2d45dd1)
     (local false)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729
       5df1715b4499126654748a00e0142c5e
       aa150dea272f13137145f9b1336e6f89
       9087f2ff68e06ebafea537cd921b2b75
       3e0806da37ceb5ee0d783c15826db0e1
       b6a38388b521e9d788bf8c241c8b7131
       1513262a352b79a04ff483cd84ebd966
       dc50e9309dbe56e9ba5a145dd0c7d272
       e7b78a2ef3334358a41a86855c8739c2
       b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ppxlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid c1f4a4a8fcca6a7045ebee5b639dc729)
     (local false)
     (requires
      (aa150dea272f13137145f9b1336e6f89 dc50e9309dbe56e9ba5a145dd0c7d272))
     (source_dir /FINDLIB/ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid aa150dea272f13137145f9b1336e6f89)
     (local false)
     (requires
      (3238f110cc121b8de5fb94811b5395f0 b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid 9087f2ff68e06ebafea537cd921b2b75)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid 1513262a352b79a04ff483cd84ebd966)
     (local false)
     (requires
      (e7b78a2ef3334358a41a86855c8739c2 dc50e9309dbe56e9ba5a145dd0c7d272))
     (source_dir /FINDLIB/ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid b6a38388b521e9d788bf8c241c8b7131)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid e7b78a2ef3334358a41a86855c8739c2)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name static_lib)
     (uid 12c434f04237f2f66660cf4ef9d28bac)
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
     (uid dc50e9309dbe56e9ba5a145dd0c7d272)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests --with-pps lib
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name compiler-libs)
     (uid 25fa301af563256248c5dadd60078c6e)
     (local false)
     (requires ())
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name compiler-libs.common)
     (uid b3f1696f67e77afbbf90bbd4ff4db3f7)
     (local false)
     (requires (25fa301af563256248c5dadd60078c6e))
     (source_dir /FINDLIB/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid d4bbc5a64c5463ce6eb84f54a61aa8b8)
     (local true)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729
       2c25d00eb24f552a85fba122b2d45dd1
       12c434f04237f2f66660cf4ef9d28bac))
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
     (uid 48f89f472eed42cfe8a7ae83e2c8c1ce)
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
     (uid 3238f110cc121b8de5fb94811b5395f0)
     (local false)
     (requires (b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid 5df1715b4499126654748a00e0142c5e)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB/ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid 3e0806da37ceb5ee0d783c15826db0e1)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB/ppx_derivers))))
   (library
    ((name ppxlib)
     (uid 2c25d00eb24f552a85fba122b2d45dd1)
     (local false)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729
       5df1715b4499126654748a00e0142c5e
       aa150dea272f13137145f9b1336e6f89
       9087f2ff68e06ebafea537cd921b2b75
       3e0806da37ceb5ee0d783c15826db0e1
       b6a38388b521e9d788bf8c241c8b7131
       1513262a352b79a04ff483cd84ebd966
       dc50e9309dbe56e9ba5a145dd0c7d272
       e7b78a2ef3334358a41a86855c8739c2
       b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ppxlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid c1f4a4a8fcca6a7045ebee5b639dc729)
     (local false)
     (requires
      (aa150dea272f13137145f9b1336e6f89 dc50e9309dbe56e9ba5a145dd0c7d272))
     (source_dir /FINDLIB/ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid aa150dea272f13137145f9b1336e6f89)
     (local false)
     (requires
      (3238f110cc121b8de5fb94811b5395f0 b3f1696f67e77afbbf90bbd4ff4db3f7))
     (source_dir /FINDLIB/ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid 9087f2ff68e06ebafea537cd921b2b75)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid 1513262a352b79a04ff483cd84ebd966)
     (local false)
     (requires
      (e7b78a2ef3334358a41a86855c8739c2 dc50e9309dbe56e9ba5a145dd0c7d272))
     (source_dir /FINDLIB/ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid b6a38388b521e9d788bf8c241c8b7131)
     (local false)
     (requires ())
     (source_dir /FINDLIB/ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB/ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid e7b78a2ef3334358a41a86855c8739c2)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name static_lib)
     (uid 12c434f04237f2f66660cf4ef9d28bac)
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
     (uid dc50e9309dbe56e9ba5a145dd0c7d272)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims)))))
