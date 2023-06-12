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
     (uid bb0491d2febd3cbb37eaaa4c3bd212e2)
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
      (c9367091ddd9a70d99fc22ede348f17c
       1f2b5eb300ea716920494385a31bb5fb
       5014e215e204cf8da6c32644cda1b31e
       249b2edaf3cc552a247667041bb5f015
       ba85adfb1c97e7d7af3df35b16b2fc0d
       2363fd46dac995a1c79679dfa1a9881b
       43b7cbe1f93f4f502ec614971027cff9
       e68a558facd1546b51c7abdbf6aed1cb
       24f4eb12e3ff51b310dbf7443c6087be
       449445be7a24ce51e119d57e9e255d3f
       5ae836dcdead11d5c16815297c5a1ae6
       2c61db8e94cb08e0fe642152aee8121a
       6fb5d46437c55abca48c8b995f8afa51
       f9851d3f8ae32391e7594cf97332a78c))
     (modules
      (((name Exe)
        (impl (_build/default/exe/exe.ml))
        (intf ())
        (cmt (_build/default/exe/.exe.eobjs/byte/dune__exe__Exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/exe/.exe.eobjs/byte))))
   (library
    ((name compiler-libs.common)
     (uid c9367091ddd9a70d99fc22ede348f17c)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ocaml/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB//ocaml/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid f9851d3f8ae32391e7594cf97332a78c)
     (local true)
     (requires
      (ba85adfb1c97e7d7af3df35b16b2fc0d
       2c61db8e94cb08e0fe642152aee8121a
       6fb5d46437c55abca48c8b995f8afa51))
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
     (uid 1f2b5eb300ea716920494385a31bb5fb)
     (local false)
     (requires (c9367091ddd9a70d99fc22ede348f17c))
     (source_dir /FINDLIB//ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB//ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid 2363fd46dac995a1c79679dfa1a9881b)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB//ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid e68a558facd1546b51c7abdbf6aed1cb)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB//ppx_derivers))))
   (library
    ((name ppxlib)
     (uid 2c61db8e94cb08e0fe642152aee8121a)
     (local false)
     (requires
      (ba85adfb1c97e7d7af3df35b16b2fc0d
       2363fd46dac995a1c79679dfa1a9881b
       5014e215e204cf8da6c32644cda1b31e
       43b7cbe1f93f4f502ec614971027cff9
       e68a558facd1546b51c7abdbf6aed1cb
       24f4eb12e3ff51b310dbf7443c6087be
       5ae836dcdead11d5c16815297c5a1ae6
       249b2edaf3cc552a247667041bb5f015
       449445be7a24ce51e119d57e9e255d3f))
     (source_dir /FINDLIB//ppxlib)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid ba85adfb1c97e7d7af3df35b16b2fc0d)
     (local false)
     (requires
      (5014e215e204cf8da6c32644cda1b31e 249b2edaf3cc552a247667041bb5f015))
     (source_dir /FINDLIB//ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid 5014e215e204cf8da6c32644cda1b31e)
     (local false)
     (requires
      (1f2b5eb300ea716920494385a31bb5fb c9367091ddd9a70d99fc22ede348f17c))
     (source_dir /FINDLIB//ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid 43b7cbe1f93f4f502ec614971027cff9)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid 5ae836dcdead11d5c16815297c5a1ae6)
     (local false)
     (requires
      (449445be7a24ce51e119d57e9e255d3f 249b2edaf3cc552a247667041bb5f015))
     (source_dir /FINDLIB//ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid 24f4eb12e3ff51b310dbf7443c6087be)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid 449445be7a24ce51e119d57e9e255d3f)
     (local false)
     (requires ())
     (source_dir /FINDLIB//sexplib0)
     (modules ())
     (include_dirs (/FINDLIB//sexplib0))))
   (library
    ((name static_lib)
     (uid 6fb5d46437c55abca48c8b995f8afa51)
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
     (uid 249b2edaf3cc552a247667041bb5f015)
     (local false)
     (requires ())
     (source_dir /FINDLIB//stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB//stdlib-shims)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests --with-pps lib
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name compiler-libs.common)
     (uid c9367091ddd9a70d99fc22ede348f17c)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ocaml/compiler-libs)
     (modules ())
     (include_dirs (/FINDLIB//ocaml/compiler-libs))))
   (library
    ((name dummy_ppx)
     (uid f9851d3f8ae32391e7594cf97332a78c)
     (local true)
     (requires
      (ba85adfb1c97e7d7af3df35b16b2fc0d
       2c61db8e94cb08e0fe642152aee8121a
       6fb5d46437c55abca48c8b995f8afa51))
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
     (uid bb0491d2febd3cbb37eaaa4c3bd212e2)
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
     (uid 1f2b5eb300ea716920494385a31bb5fb)
     (local false)
     (requires (c9367091ddd9a70d99fc22ede348f17c))
     (source_dir /FINDLIB//ocaml-compiler-libs/common)
     (modules ())
     (include_dirs (/FINDLIB//ocaml-compiler-libs/common))))
   (library
    ((name ocaml-compiler-libs.shadow)
     (uid 2363fd46dac995a1c79679dfa1a9881b)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ocaml-compiler-libs/shadow)
     (modules ())
     (include_dirs (/FINDLIB//ocaml-compiler-libs/shadow))))
   (library
    ((name ppx_derivers)
     (uid e68a558facd1546b51c7abdbf6aed1cb)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_derivers)
     (modules ())
     (include_dirs (/FINDLIB//ppx_derivers))))
   (library
    ((name ppxlib)
     (uid 2c61db8e94cb08e0fe642152aee8121a)
     (local false)
     (requires
      (ba85adfb1c97e7d7af3df35b16b2fc0d
       2363fd46dac995a1c79679dfa1a9881b
       5014e215e204cf8da6c32644cda1b31e
       43b7cbe1f93f4f502ec614971027cff9
       e68a558facd1546b51c7abdbf6aed1cb
       24f4eb12e3ff51b310dbf7443c6087be
       5ae836dcdead11d5c16815297c5a1ae6
       249b2edaf3cc552a247667041bb5f015
       449445be7a24ce51e119d57e9e255d3f))
     (source_dir /FINDLIB//ppxlib)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib))))
   (library
    ((name ppxlib.ast)
     (uid ba85adfb1c97e7d7af3df35b16b2fc0d)
     (local false)
     (requires
      (5014e215e204cf8da6c32644cda1b31e 249b2edaf3cc552a247667041bb5f015))
     (source_dir /FINDLIB//ppxlib/ast)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/ast))))
   (library
    ((name ppxlib.astlib)
     (uid 5014e215e204cf8da6c32644cda1b31e)
     (local false)
     (requires
      (1f2b5eb300ea716920494385a31bb5fb c9367091ddd9a70d99fc22ede348f17c))
     (source_dir /FINDLIB//ppxlib/astlib)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/astlib))))
   (library
    ((name ppxlib.print_diff)
     (uid 43b7cbe1f93f4f502ec614971027cff9)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppxlib/print_diff)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/print_diff))))
   (library
    ((name ppxlib.stdppx)
     (uid 5ae836dcdead11d5c16815297c5a1ae6)
     (local false)
     (requires
      (449445be7a24ce51e119d57e9e255d3f 249b2edaf3cc552a247667041bb5f015))
     (source_dir /FINDLIB//ppxlib/stdppx)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/stdppx))))
   (library
    ((name ppxlib.traverse_builtins)
     (uid 24f4eb12e3ff51b310dbf7443c6087be)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppxlib/traverse_builtins)
     (modules ())
     (include_dirs (/FINDLIB//ppxlib/traverse_builtins))))
   (library
    ((name sexplib0)
     (uid 449445be7a24ce51e119d57e9e255d3f)
     (local false)
     (requires ())
     (source_dir /FINDLIB//sexplib0)
     (modules ())
     (include_dirs (/FINDLIB//sexplib0))))
   (library
    ((name static_lib)
     (uid 6fb5d46437c55abca48c8b995f8afa51)
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
     (uid 249b2edaf3cc552a247667041bb5f015)
     (local false)
     (requires ())
     (source_dir /FINDLIB//stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB//stdlib-shims)))))
