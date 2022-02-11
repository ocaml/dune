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
  >   (name bar)
  >   (preprocess (pps ppx_inline_test))
  >   (modules bar bar2))
  > 
  > (executable
  >  (name main2)
  >  (libraries foo_x foo)
  >  (modules main2 main2_aux1 main2_aux2 main2_aux3 main2_aux4)
  >  (modules_without_implementation main2_aux4))
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
   (library
    ((name bar)
     (uid 97586d5adea44246d88d31b0f6e340ed)
     (local true)
     (requires (34d24431bb62c3c8c9933acac64a4fb8))
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
    ((name ppx_inline_test.runtime-lib)
     (uid 34d24431bb62c3c8c9933acac64a4fb8)
     (local false)
     (requires
      (3bb762fc176df327f9073eba2bc8dc2b
       5793e5a96f11c93185b5422a5edc7b05
       319b80b484fe99fb8019ed8ff7892b5c))
     (source_dir /OCAML_ROOT/lib/ppx_inline_test/runtime-lib)
     (modules ())
     (include_dirs (/OCAML_ROOT/lib/ppx_inline_test/runtime-lib)))))

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
   (library
    ((name bar)
     (uid 97586d5adea44246d88d31b0f6e340ed)
     (local true)
     (requires (34d24431bb62c3c8c9933acac64a4fb8))
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
    ((name ppx_inline_test.runtime-lib)
     (uid 34d24431bb62c3c8c9933acac64a4fb8)
     (local false)
     (requires
      (3bb762fc176df327f9073eba2bc8dc2b
       5793e5a96f11c93185b5422a5edc7b05
       319b80b484fe99fb8019ed8ff7892b5c))
     (source_dir /OCAML_ROOT/lib/ppx_inline_test/runtime-lib)
     (modules ())
     (include_dirs (/OCAML_ROOT/lib/ppx_inline_test/runtime-lib)))))


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
