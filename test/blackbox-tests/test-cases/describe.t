Test for the `dune describe` command
====================================

Setup
-----

  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (package
  >  (name foo)
  >  (synopsis "foo bar baz"))
  > (generate_opam_files)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foo)
  >  (libraries foo.x)
  >  (modules foo))
  > 
  > (library
  >  (name foo_x)
  >  (public_name foo.x)
  >  (modules foo_x))
  > 
  > (executable
  >  (name main)
  >  (libraries foo_x foo)
  >  (modules main))
  > 
  > (library
  >  (name bar)
  >  (preprocess (pps ppx_inline_test))
  >  (modules bar bar2))
  > 
  > (executable
  >  (name main2)
  >  (libraries foo_x foo)
  >  (modules main2 main2_aux1 main2_aux2 main2_aux3 main2_aux4)
  >  (modules_without_implementation main2_aux4))
  > 
  > (executable
  >   (name main3)
  >   (libraries cmdliner)
  >   (modules main3))
  > EOF

  $ touch foo.ml
  $ touch foo_x.ml
  $ touch main.ml

  $ cat >bar.ml <<EOF
  > let x = Bar2.x
  > let%test _ = (x = 42)
  > EOF

  $ cat >bar2.ml <<EOF
  > let x = 42
  > EOF

  $ cat >main2.ml <<EOF
  > let x = Main2_aux1.x
  > EOF

  $ cat >main2_aux1.ml <<EOF
  > let x = Main2_aux2.x
  > let y : Main2_aux4.t = Main2_aux2.x
  > EOF

  $ cat >main2_aux1.mli <<EOF
  > val x: Main2_aux3.t
  > val y: Main2_aux4.t
  > EOF

  $ cat >main2_aux2.ml <<EOF
  > let x = 0
  > EOF

  $ cat >main2_aux3.ml <<EOF
  > type t = int
  > EOF

  $ cat >main2_aux3.mli <<EOF
  > type t = int
  > EOF

  $ cat >main2_aux4.mli <<EOF
  > type t = int
  > EOF

  $ touch main3.ml

Describe various things
-----------------------

Warning: when testing the ``dune describe workspace`` command, do not
forget to pass the ``--sanitize-for-tests`` flags, so that the tests
are reproducible, and are kept consistent between different machines.
``dune describe workspace`` may indeed print absolute paths, that are
not stable across different setups.

  $ dune describe workspace --lang 0.1 --sanitize-for-tests
  ((executables
    ((names (main))
     (requires
      (c17373aee51bab94097b4b7818553cf3 5dd4bd87ad37b4f5713085aff4bee9c9))
     (modules
      (((name Main)
        (impl (_build/default/main.ml))
        (intf ())
        (cmt (_build/default/.main.eobjs/byte/dune__exe__Main.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.main.eobjs/byte))))
   (executables
    ((names (main2))
     (requires
      (c17373aee51bab94097b4b7818553cf3 5dd4bd87ad37b4f5713085aff4bee9c9))
     (modules
      (((name Main2_aux4)
        (impl ())
        (intf (_build/default/main2_aux4.mli))
        (cmt ())
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux4.cmti)))
       ((name Main2_aux3)
        (impl (_build/default/main2_aux3.ml))
        (intf (_build/default/main2_aux3.mli))
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux3.cmt))
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux3.cmti)))
       ((name Main2_aux2)
        (impl (_build/default/main2_aux2.ml))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux2.cmt))
        (cmti ()))
       ((name Main2_aux1)
        (impl (_build/default/main2_aux1.ml))
        (intf (_build/default/main2_aux1.mli))
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux1.cmt))
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux1.cmti)))
       ((name Main2)
        (impl (_build/default/main2.ml))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2.cmt))
        (cmti ()))
       ((name Dune__exe)
        (impl (_build/default/.main2.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.main2.eobjs/byte))))
   (executables
    ((names (main3))
     (requires (c480a7c584d174c22d86dbdb79515d7d))
     (modules
      (((name Main3)
        (impl (_build/default/main3.ml))
        (intf ())
        (cmt (_build/default/.main3.eobjs/byte/dune__exe__Main3.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.main3.eobjs/byte))))
   (library
    ((name bar)
     (uid 97586d5adea44246d88d31b0f6e340ed)
     (local true)
     (requires (55b023c301c18e021a22384b996d66af))
     (source_dir _build/default)
     (modules
      (((name Bar2)
        (impl (_build/default/bar2.ml))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar__Bar2.cmt))
        (cmti ()))
       ((name Bar)
        (impl (_build/default/bar.ml))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar.cmt))
        (cmti ()))
       ((name Bar__)
        (impl (_build/default/bar__.ml-gen))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar__.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.bar.objs/byte))))
   (library
    ((name base)
     (uid 46774e2b7a404647f12956126bd28f95)
     (local false)
     (requires
      (3803213fe04d38cea3157f466b9a8747
       ce20dcd0c4bae81524a96662594adde2
       449445be7a24ce51e119d57e9e255d3f
       b91de1a8b6b882f5b4726d5b1f2ece6e))
     (source_dir /FINDLIB//base)
     (modules ())
     (include_dirs (/FINDLIB//base))))
   (library
    ((name base.base_internalhash_types)
     (uid 3803213fe04d38cea3157f466b9a8747)
     (local false)
     (requires ())
     (source_dir /FINDLIB//base/base_internalhash_types)
     (modules ())
     (include_dirs (/FINDLIB//base/base_internalhash_types))))
   (library
    ((name base.caml)
     (uid ce20dcd0c4bae81524a96662594adde2)
     (local false)
     (requires ())
     (source_dir /FINDLIB//base/caml)
     (modules ())
     (include_dirs (/FINDLIB//base/caml))))
   (library
    ((name base.shadow_stdlib)
     (uid b91de1a8b6b882f5b4726d5b1f2ece6e)
     (local false)
     (requires (ce20dcd0c4bae81524a96662594adde2))
     (source_dir /FINDLIB//base/shadow_stdlib)
     (modules ())
     (include_dirs (/FINDLIB//base/shadow_stdlib))))
   (library
    ((name cmdliner)
     (uid c480a7c584d174c22d86dbdb79515d7d)
     (local false)
     (requires ())
     (source_dir /FINDLIB//cmdliner)
     (modules ())
     (include_dirs (/FINDLIB//cmdliner))))
   (library
    ((name foo)
     (uid 5dd4bd87ad37b4f5713085aff4bee9c9)
     (local true)
     (requires (c17373aee51bab94097b4b7818553cf3))
     (source_dir _build/default)
     (modules
      (((name Foo)
        (impl (_build/default/foo.ml))
        (intf ())
        (cmt (_build/default/.foo.objs/byte/foo.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.foo.objs/byte))))
   (library
    ((name foo.x)
     (uid c17373aee51bab94097b4b7818553cf3)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Foo_x)
        (impl (_build/default/foo_x.ml))
        (intf ())
        (cmt (_build/default/.foo_x.objs/byte/foo_x.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.foo_x.objs/byte))))
   (library
    ((name jane-street-headers)
     (uid 73ad4e016c34da2f2d4a1cff930ac883)
     (local false)
     (requires ())
     (source_dir /FINDLIB//jane-street-headers)
     (modules ())
     (include_dirs (/FINDLIB//jane-street-headers))))
   (library
    ((name ppx_compare.runtime-lib)
     (uid 708bf5748829e3636236f5d8c610f430)
     (local false)
     (requires (46774e2b7a404647f12956126bd28f95))
     (source_dir /FINDLIB//ppx_compare/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_compare/runtime-lib))))
   (library
    ((name ppx_enumerate.runtime-lib)
     (uid 2c6f959289bddfd3b3ada8f64a3ca5d8)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_enumerate/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_enumerate/runtime-lib))))
   (library
    ((name ppx_hash.runtime-lib)
     (uid 92685e7ac0dd1fa9cd96be221032349e)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       475f353b2705e034b0287d7ffe9e5225
       708bf5748829e3636236f5d8c610f430))
     (source_dir /FINDLIB//ppx_hash/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_hash/runtime-lib))))
   (library
    ((name ppx_inline_test.config)
     (uid 46e75006466e7a020139d86575978cb3)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_inline_test/config)
     (modules ())
     (include_dirs (/FINDLIB//ppx_inline_test/config))))
   (library
    ((name ppx_inline_test.runtime-lib)
     (uid 55b023c301c18e021a22384b996d66af)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       46e75006466e7a020139d86575978cb3
       66c5927ddd01193cdcfac8ce97e58e63))
     (source_dir /FINDLIB//ppx_inline_test/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_inline_test/runtime-lib))))
   (library
    ((name ppx_sexp_conv.runtime-lib)
     (uid 475f353b2705e034b0287d7ffe9e5225)
     (local false)
     (requires (449445be7a24ce51e119d57e9e255d3f))
     (source_dir /FINDLIB//ppx_sexp_conv/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_sexp_conv/runtime-lib))))
   (library
    ((name sexplib0)
     (uid 449445be7a24ce51e119d57e9e255d3f)
     (local false)
     (requires ())
     (source_dir /FINDLIB//sexplib0)
     (modules ())
     (include_dirs (/FINDLIB//sexplib0))))
   (library
    ((name time_now)
     (uid 66c5927ddd01193cdcfac8ce97e58e63)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       73ad4e016c34da2f2d4a1cff930ac883
       475f353b2705e034b0287d7ffe9e5225
       708bf5748829e3636236f5d8c610f430
       2c6f959289bddfd3b3ada8f64a3ca5d8
       92685e7ac0dd1fa9cd96be221032349e))
     (source_dir /FINDLIB//time_now)
     (modules ())
     (include_dirs (/FINDLIB//time_now)))))

  $ dune describe workspace --lang 0.1 --with-deps --sanitize-for-tests
  ((executables
    ((names (main))
     (requires
      (c17373aee51bab94097b4b7818553cf3 5dd4bd87ad37b4f5713085aff4bee9c9))
     (modules
      (((name Main)
        (impl (_build/default/main.ml))
        (intf ())
        (cmt (_build/default/.main.eobjs/byte/dune__exe__Main.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.main.eobjs/byte))))
   (executables
    ((names (main2))
     (requires
      (c17373aee51bab94097b4b7818553cf3 5dd4bd87ad37b4f5713085aff4bee9c9))
     (modules
      (((name Main2_aux4)
        (impl ())
        (intf (_build/default/main2_aux4.mli))
        (cmt ())
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux4.cmti))
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Main2_aux3)
        (impl (_build/default/main2_aux3.ml))
        (intf (_build/default/main2_aux3.mli))
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux3.cmt))
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux3.cmti))
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Main2_aux2)
        (impl (_build/default/main2_aux2.ml))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Main2_aux1)
        (impl (_build/default/main2_aux1.ml))
        (intf (_build/default/main2_aux1.mli))
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux1.cmt))
        (cmti (_build/default/.main2.eobjs/byte/dune__exe__Main2_aux1.cmti))
        (module_deps
         ((for_intf
           (Main2_aux3 Main2_aux4))
          (for_impl
           (Main2_aux2 Main2_aux4)))))
       ((name Main2)
        (impl (_build/default/main2.ml))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe__Main2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (Main2_aux1)))))
       ((name Dune__exe)
        (impl (_build/default/.main2.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.main2.eobjs/byte/dune__exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.main2.eobjs/byte))))
   (executables
    ((names (main3))
     (requires (c480a7c584d174c22d86dbdb79515d7d))
     (modules
      (((name Main3)
        (impl (_build/default/main3.ml))
        (intf ())
        (cmt (_build/default/.main3.eobjs/byte/dune__exe__Main3.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.main3.eobjs/byte))))
   (library
    ((name bar)
     (uid 97586d5adea44246d88d31b0f6e340ed)
     (local true)
     (requires (55b023c301c18e021a22384b996d66af))
     (source_dir _build/default)
     (modules
      (((name Bar2)
        (impl (_build/default/bar2.ml))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar__Bar2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Bar)
        (impl (_build/default/bar.ml))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (Bar2)))))
       ((name Bar__)
        (impl (_build/default/bar__.ml-gen))
        (intf ())
        (cmt (_build/default/.bar.objs/byte/bar__.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.bar.objs/byte))))
   (library
    ((name base)
     (uid 46774e2b7a404647f12956126bd28f95)
     (local false)
     (requires
      (3803213fe04d38cea3157f466b9a8747
       ce20dcd0c4bae81524a96662594adde2
       449445be7a24ce51e119d57e9e255d3f
       b91de1a8b6b882f5b4726d5b1f2ece6e))
     (source_dir /FINDLIB//base)
     (modules ())
     (include_dirs (/FINDLIB//base))))
   (library
    ((name base.base_internalhash_types)
     (uid 3803213fe04d38cea3157f466b9a8747)
     (local false)
     (requires ())
     (source_dir /FINDLIB//base/base_internalhash_types)
     (modules ())
     (include_dirs (/FINDLIB//base/base_internalhash_types))))
   (library
    ((name base.caml)
     (uid ce20dcd0c4bae81524a96662594adde2)
     (local false)
     (requires ())
     (source_dir /FINDLIB//base/caml)
     (modules ())
     (include_dirs (/FINDLIB//base/caml))))
   (library
    ((name base.shadow_stdlib)
     (uid b91de1a8b6b882f5b4726d5b1f2ece6e)
     (local false)
     (requires (ce20dcd0c4bae81524a96662594adde2))
     (source_dir /FINDLIB//base/shadow_stdlib)
     (modules ())
     (include_dirs (/FINDLIB//base/shadow_stdlib))))
   (library
    ((name cmdliner)
     (uid c480a7c584d174c22d86dbdb79515d7d)
     (local false)
     (requires ())
     (source_dir /FINDLIB//cmdliner)
     (modules ())
     (include_dirs (/FINDLIB//cmdliner))))
   (library
    ((name foo)
     (uid 5dd4bd87ad37b4f5713085aff4bee9c9)
     (local true)
     (requires (c17373aee51bab94097b4b7818553cf3))
     (source_dir _build/default)
     (modules
      (((name Foo)
        (impl (_build/default/foo.ml))
        (intf ())
        (cmt (_build/default/.foo.objs/byte/foo.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.foo.objs/byte))))
   (library
    ((name foo.x)
     (uid c17373aee51bab94097b4b7818553cf3)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Foo_x)
        (impl (_build/default/foo_x.ml))
        (intf ())
        (cmt (_build/default/.foo_x.objs/byte/foo_x.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.foo_x.objs/byte))))
   (library
    ((name jane-street-headers)
     (uid 73ad4e016c34da2f2d4a1cff930ac883)
     (local false)
     (requires ())
     (source_dir /FINDLIB//jane-street-headers)
     (modules ())
     (include_dirs (/FINDLIB//jane-street-headers))))
   (library
    ((name ppx_compare.runtime-lib)
     (uid 708bf5748829e3636236f5d8c610f430)
     (local false)
     (requires (46774e2b7a404647f12956126bd28f95))
     (source_dir /FINDLIB//ppx_compare/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_compare/runtime-lib))))
   (library
    ((name ppx_enumerate.runtime-lib)
     (uid 2c6f959289bddfd3b3ada8f64a3ca5d8)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_enumerate/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_enumerate/runtime-lib))))
   (library
    ((name ppx_hash.runtime-lib)
     (uid 92685e7ac0dd1fa9cd96be221032349e)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       475f353b2705e034b0287d7ffe9e5225
       708bf5748829e3636236f5d8c610f430))
     (source_dir /FINDLIB//ppx_hash/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_hash/runtime-lib))))
   (library
    ((name ppx_inline_test.config)
     (uid 46e75006466e7a020139d86575978cb3)
     (local false)
     (requires ())
     (source_dir /FINDLIB//ppx_inline_test/config)
     (modules ())
     (include_dirs (/FINDLIB//ppx_inline_test/config))))
   (library
    ((name ppx_inline_test.runtime-lib)
     (uid 55b023c301c18e021a22384b996d66af)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       46e75006466e7a020139d86575978cb3
       66c5927ddd01193cdcfac8ce97e58e63))
     (source_dir /FINDLIB//ppx_inline_test/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_inline_test/runtime-lib))))
   (library
    ((name ppx_sexp_conv.runtime-lib)
     (uid 475f353b2705e034b0287d7ffe9e5225)
     (local false)
     (requires (449445be7a24ce51e119d57e9e255d3f))
     (source_dir /FINDLIB//ppx_sexp_conv/runtime-lib)
     (modules ())
     (include_dirs (/FINDLIB//ppx_sexp_conv/runtime-lib))))
   (library
    ((name sexplib0)
     (uid 449445be7a24ce51e119d57e9e255d3f)
     (local false)
     (requires ())
     (source_dir /FINDLIB//sexplib0)
     (modules ())
     (include_dirs (/FINDLIB//sexplib0))))
   (library
    ((name time_now)
     (uid 66c5927ddd01193cdcfac8ce97e58e63)
     (local false)
     (requires
      (46774e2b7a404647f12956126bd28f95
       73ad4e016c34da2f2d4a1cff930ac883
       475f353b2705e034b0287d7ffe9e5225
       708bf5748829e3636236f5d8c610f430
       2c6f959289bddfd3b3ada8f64a3ca5d8
       92685e7ac0dd1fa9cd96be221032349e))
     (source_dir /FINDLIB//time_now)
     (modules ())
     (include_dirs (/FINDLIB//time_now)))))


Test other formats
------------------

  $ dune describe workspace --format csexp --lang 0.1 --sanitize-for-tests | cut -c 1-85
  ((11:executables((5:names(4:main))(8:requires(32:c17373aee51bab94097b4b7818553cf332:5

Test errors
-----------

  $ dune describe --lang 0.1 workspac
  Error: Unknown constructor workspac
  Hint: did you mean workspace?
  [1]

  $ dune describe --lang 0.1 workspace xxx
  Error: Too many argument for workspace
  [1]

  $ dune describe --lang 1.0
  dune describe: Only --lang 0.1 is available at the moment as this command is not yet
                 stabilised. If you would like to release a software that relies on the output
                 of 'dune describe', please open a ticket on
                 https://github.com/ocaml/dune.
  Usage: dune describe [OPTION]... [STRING]...
  Try `dune describe --help' or `dune --help' for more information.
  [1]

opam file listing
-----------------

  $ dune describe --lang 0.1 opam-files | dune_cmd expand_lines
  ((foo.opam
    "# This file is generated by dune, edit dune-project instead
  opam-version: \"2.0\"
  synopsis: \"foo bar baz\"
  depends: [
    \"dune\" {>= \"2.3\"}
  ]
  build: [
    [\"dune\" \"subst\"] {pinned}
    [
      \"dune\"
      \"build\"
      \"-p\"
      name
      \"-j\"
      jobs
      \"@install\"
      \"@runtest\" {with-test}
      \"@doc\" {with-doc}
    ]
  ]
  "))
