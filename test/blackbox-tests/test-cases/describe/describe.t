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
  > (env
  >  (_ (binaries refmt.exe)))
  > 
  > (executable
  >  (name refmt)
  >  (modules refmt))
  > 
  > (library
  >  (name dummy_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (modules dummy_ppx))
  > 
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
  >  (preprocess (pps dummy_ppx))
  >  (modules bar bar2))
  > 
  > (executable
  >  (name main2)
  >  (libraries foo_x foo)
  >  (modules main2 main2_aux1 main2_aux2 main2_aux3 main2_aux4)
  >  (modules_without_implementation main2_aux4))
  > 
  > (executable
  >  (name main3)
  >  (libraries cmdliner)
  >  (modules main3))
  > 
  > (library
  >  (name per_module_pp_lib)
  >  (modules pp1 pp2)
  >  (preprocess (per_module ((pps dummy_ppx) pp2))))
  > 
  > (executable
  >  (name per_module_pp_exe)
  >  (modules per_module_pp_exe pp3 pp4)
  >  (preprocess (per_module ((pps dummy_ppx) pp4))))
  > 
  > (library
  >  (name per_module_action_lib)
  >  (modules action1 action2)
  >  (preprocess (per_module ((action (cat %{input-file})) action2))))
  > 
  > (library
  >  (name per_module_action_exe)
  >  (modules per_module_action_exe action3 action4)
  >  (preprocess (per_module ((action (cat %{input-file})) action4))))
  > 
  > (executable
  >  (name re_exe)
  >  (modules re_exe re_exe1 re_exe2)
  >  (preprocess (per_module ((pps dummy_ppx) re_exe1))))
  > 
  > (library
  >  (name re_lib)
  >  (modules re_lib1 re_lib2)
  >  (preprocess (per_module ((pps dummy_ppx) re_lib1))))
  > EOF

  $ touch refmt.ml

  $ cat >dummy_ppx.ml <<EOF
  > (* dummy PPX rewriter, for use in tests *)
  > let () =
  >   Ppxlib.Driver.register_transformation
  >     "dummy"
  >     ~impl:(fun s -> s)
  > EOF

  $ touch foo.ml
  $ touch foo_x.ml
  $ touch main.ml

  $ cat >bar.ml <<EOF
  > let x = Bar2.x
  > let%dummy _ = (x = 42)
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

  $ cat >pp1.ml <<EOF
  > let x = 0
  > EOF

  $ cat >pp2.ml <<EOF
  > let%dummy _ = (Pp1.x = 0)
  > let y = 0
  > EOF

  $ cat >pp4.ml <<EOF
  > type t =
  > | Foo
  > | Bar of bool
  > [@@deriving enumerate]
  > EOF

  $ cat >pp3.ml <<EOF
  > let foo = Pp4.foo
  > EOF

  $ cat >per_module_pp_exe.ml <<EOF
  > let () = assert (List.mem Pp3.foo Pp4.all)
  > EOF

  $ cat >action1.ml <<EOF
  > let x = 0
  > EOF

  $ cat >action2.ml <<EOF
  > let y = Action1.x
  > EOF

  $ cp action1.ml action3.ml
  $ cp action2.ml action4.ml
  $ cat >per_module_action_exe.ml <<EOF
  > let () = assert (Action3.x = Action4.y)
  > EOF

  $ touch re_exe.re re_exe1.re re_exe1.rei re_exe2.re

  $ touch re_lib1.re re_lib2.re re_lib2.rei

  $ mkdir virtual
  $ cat >virtual/dune <<EOF
  > (library
  >  (name virtual)
  >  (virtual_modules virtual))
  > EOF
  $ touch virtual/virtual.mli
  $ mkdir virtual_impl1
  $ cat >virtual_impl1/dune <<EOF
  > (library
  >  (name virtual_impl1)
  >  (implements virtual))
  > EOF
  $ touch virtual_impl1/virtual.ml
  $ mkdir virtual_impl2
  $ cat >virtual_impl2/dune <<EOF
  > (library
  >  (name virtual_impl2)
  >  (implements virtual))
  > EOF
  $ touch virtual_impl2/virtual.ml

  $ mkdir subdir
  $ mkdir subdir/subfolder
  $ cat >subdir/subfolder/dune <<EOF
  > (library
  >  (name subfolder_lib))
  > EOF
  $ touch subdir/subfolder/subfolder_lib.ml


Describe various things
-----------------------

Warning: when testing the ``dune describe workspace`` command, do not
forget to pass the ``--sanitize-for-tests`` flags, so that the tests
are reproducible, and are kept consistent between different machines.
``dune describe workspace`` may indeed print absolute paths, that are
not stable across different setups.

  $ dune describe workspace --lang 0.1 --sanitize-for-tests
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (refmt))
     (requires ())
     (modules
      (((name Refmt)
        (impl (_build/default/refmt.ml))
        (intf ())
        (cmt (_build/default/.refmt.eobjs/byte/dune__exe__Refmt.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.refmt.eobjs/byte))))
   (executables
    ((names (main))
     (requires
      (5a9c5cdc441305690411419a4a5e3f34 536b4af2ace976f46343f6b47c2c32f2))
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
      (5a9c5cdc441305690411419a4a5e3f34 536b4af2ace976f46343f6b47c2c32f2))
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
     (requires (a392325be68958876f9598a822187d55))
     (modules
      (((name Main3)
        (impl (_build/default/main3.ml))
        (intf ())
        (cmt (_build/default/.main3.eobjs/byte/dune__exe__Main3.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.main3.eobjs/byte))))
   (executables
    ((names (per_module_pp_exe))
     (requires ())
     (modules
      (((name Pp4)
        (impl (_build/default/pp4.ml))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Pp4.cmt))
        (cmti ()))
       ((name Pp3)
        (impl (_build/default/pp3.ml))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Pp3.cmt))
        (cmti ()))
       ((name Per_module_pp_exe)
        (impl (_build/default/per_module_pp_exe.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Per_module_pp_exe.cmt))
        (cmti ()))
       ((name Dune__exe)
        (impl (_build/default/.per_module_pp_exe.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.per_module_pp_exe.eobjs/byte))))
   (executables
    ((names (re_exe))
     (requires ())
     (modules
      (((name Re_exe2)
        (impl (_build/default/re_exe2.re))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe2.cmt))
        (cmti ()))
       ((name Re_exe1)
        (impl (_build/default/re_exe1.re))
        (intf (_build/default/re_exe1.rei))
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe1.cmt))
        (cmti (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe1.cmti)))
       ((name Re_exe)
        (impl (_build/default/re_exe.re))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe.cmt))
        (cmti ()))
       ((name Dune__exe)
        (impl (_build/default/.re_exe.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.re_exe.eobjs/byte))))
   (library
    ((name bar)
     (uid 7dc7853647f347467f62c91edc898b00)
     (local true)
     (requires ())
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
    ((name cmdliner)
     (uid a392325be68958876f9598a822187d55)
     (local false)
     (requires ())
     (source_dir /FINDLIB/cmdliner)
     (modules ())
     (include_dirs (/FINDLIB/cmdliner))))
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
     (uid ff1580960f48a57430e95cef762bf472)
     (local true)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729 2c25d00eb24f552a85fba122b2d45dd1))
     (source_dir _build/default)
     (modules
      (((name Dummy_ppx)
        (impl (_build/default/dummy_ppx.ml))
        (intf ())
        (cmt (_build/default/.dummy_ppx.objs/byte/dummy_ppx.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.dummy_ppx.objs/byte))))
   (library
    ((name foo)
     (uid 536b4af2ace976f46343f6b47c2c32f2)
     (local true)
     (requires (5a9c5cdc441305690411419a4a5e3f34))
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
     (uid 5a9c5cdc441305690411419a4a5e3f34)
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
    ((name per_module_action_exe)
     (uid fa77b53b2ba88e3d4371707df82dbd13)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Per_module_action_exe)
        (impl (_build/default/per_module_action_exe.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe.cmt))
        (cmti ()))
       ((name Action4)
        (impl (_build/default/action4.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__Action4.cmt))
        (cmti ()))
       ((name Action3)
        (impl (_build/default/action3.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__Action3.cmt))
        (cmti ()))
       ((name Per_module_action_exe__)
        (impl (_build/default/per_module_action_exe__.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.per_module_action_exe.objs/byte))))
   (library
    ((name per_module_action_lib)
     (uid 40a11de60c0aa7eb6cbe99a43d4acc3e)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Action2)
        (impl (_build/default/action2.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib__Action2.cmt))
        (cmti ()))
       ((name Action1)
        (impl (_build/default/action1.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib__Action1.cmt))
        (cmti ()))
       ((name Per_module_action_lib)
        (impl (_build/default/per_module_action_lib.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.per_module_action_lib.objs/byte))))
   (library
    ((name per_module_pp_lib)
     (uid 5e6b5e06d15bbba69a481ad64dbed48d)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Pp2)
        (impl (_build/default/pp2.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib__Pp2.cmt))
        (cmti ()))
       ((name Pp1)
        (impl (_build/default/pp1.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib__Pp1.cmt))
        (cmti ()))
       ((name Per_module_pp_lib)
        (impl (_build/default/per_module_pp_lib.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.per_module_pp_lib.objs/byte))))
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
    ((name re_lib)
     (uid 70105dd95724ac832af7c89df2e2ff65)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Re_lib2)
        (impl (_build/default/re_lib2.re))
        (intf (_build/default/re_lib2.rei))
        (cmt (_build/default/.re_lib.objs/byte/re_lib__Re_lib2.cmt))
        (cmti (_build/default/.re_lib.objs/byte/re_lib__Re_lib2.cmti)))
       ((name Re_lib1)
        (impl (_build/default/re_lib1.re))
        (intf ())
        (cmt (_build/default/.re_lib.objs/byte/re_lib__Re_lib1.cmt))
        (cmti ()))
       ((name Re_lib)
        (impl (_build/default/re_lib.ml-gen))
        (intf ())
        (cmt (_build/default/.re_lib.objs/byte/re_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.re_lib.objs/byte))))
   (library
    ((name sexplib0)
     (uid e7b78a2ef3334358a41a86855c8739c2)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name stdlib-shims)
     (uid dc50e9309dbe56e9ba5a145dd0c7d272)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims))))
   (library
    ((name subfolder_lib)
     (uid 3ce0ac7acd90d422db7ffc54ebbdee57)
     (local true)
     (requires ())
     (source_dir _build/default/subdir/subfolder)
     (modules
      (((name Subfolder_lib)
        (impl (_build/default/subdir/subfolder/subfolder_lib.ml))
        (intf ())
        (cmt
         (_build/default/subdir/subfolder/.subfolder_lib.objs/byte/subfolder_lib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/subdir/subfolder/.subfolder_lib.objs/byte))))
   (library
    ((name virtual)
     (uid f6cee13b744e81f71621346d08c8df22)
     (local true)
     (requires ())
     (source_dir _build/default/virtual)
     (modules
      (((name Virtual)
        (impl ())
        (intf (_build/default/virtual/virtual.mli))
        (cmt ())
        (cmti (_build/default/virtual/.virtual.objs/byte/virtual.cmti)))))
     (include_dirs (_build/default/virtual/.virtual.objs/byte))))
   (library
    ((name virtual_impl1)
     (uid 9bf1effa3fd6524dd5b76957e6245f85)
     (local true)
     (requires (f6cee13b744e81f71621346d08c8df22))
     (source_dir _build/default/virtual_impl1)
     (modules
      (((name Virtual)
        (impl (_build/default/virtual_impl1/virtual.ml))
        (intf ())
        (cmt
         (_build/default/virtual_impl1/.virtual_impl1.objs/byte/virtual.cmt))
        (cmti ()))
       ((name Virtual__virtual_impl1__)
        (impl (_build/default/virtual_impl1/virtual__virtual_impl1__.ml-gen))
        (intf ())
        (cmt
         (_build/default/virtual_impl1/.virtual_impl1.objs/byte/virtual__virtual_impl1__.cmt))
        (cmti ()))))
     (include_dirs (_build/default/virtual_impl1/.virtual_impl1.objs/byte))))
   (library
    ((name virtual_impl2)
     (uid 8fd0490dfc3b56d3874fbbf6eb4abbac)
     (local true)
     (requires (f6cee13b744e81f71621346d08c8df22))
     (source_dir _build/default/virtual_impl2)
     (modules
      (((name Virtual)
        (impl (_build/default/virtual_impl2/virtual.ml))
        (intf ())
        (cmt
         (_build/default/virtual_impl2/.virtual_impl2.objs/byte/virtual.cmt))
        (cmti ()))
       ((name Virtual__virtual_impl2__)
        (impl (_build/default/virtual_impl2/virtual__virtual_impl2__.ml-gen))
        (intf ())
        (cmt
         (_build/default/virtual_impl2/.virtual_impl2.objs/byte/virtual__virtual_impl2__.cmt))
        (cmti ()))))
     (include_dirs (_build/default/virtual_impl2/.virtual_impl2.objs/byte)))))

  $ dune describe workspace --lang 0.1 --with-deps --sanitize-for-tests
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (refmt))
     (requires ())
     (modules
      (((name Refmt)
        (impl (_build/default/refmt.ml))
        (intf ())
        (cmt (_build/default/.refmt.eobjs/byte/dune__exe__Refmt.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.refmt.eobjs/byte))))
   (executables
    ((names (main))
     (requires
      (5a9c5cdc441305690411419a4a5e3f34 536b4af2ace976f46343f6b47c2c32f2))
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
      (5a9c5cdc441305690411419a4a5e3f34 536b4af2ace976f46343f6b47c2c32f2))
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
     (requires (a392325be68958876f9598a822187d55))
     (modules
      (((name Main3)
        (impl (_build/default/main3.ml))
        (intf ())
        (cmt (_build/default/.main3.eobjs/byte/dune__exe__Main3.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.main3.eobjs/byte))))
   (executables
    ((names (per_module_pp_exe))
     (requires ())
     (modules
      (((name Pp4)
        (impl (_build/default/pp4.ml))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Pp4.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Pp3)
        (impl (_build/default/pp3.ml))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Pp3.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (Pp4)))))
       ((name Per_module_pp_exe)
        (impl (_build/default/per_module_pp_exe.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe__Per_module_pp_exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl
           (Pp3 Pp4)))))
       ((name Dune__exe)
        (impl (_build/default/.per_module_pp_exe.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.per_module_pp_exe.eobjs/byte/dune__exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.per_module_pp_exe.eobjs/byte))))
   (executables
    ((names (re_exe))
     (requires ())
     (modules
      (((name Re_exe2)
        (impl (_build/default/re_exe2.re))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Re_exe1)
        (impl (_build/default/re_exe1.re))
        (intf (_build/default/re_exe1.rei))
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe1.cmt))
        (cmti (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe1.cmti))
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Re_exe)
        (impl (_build/default/re_exe.re))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe__Re_exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Dune__exe)
        (impl (_build/default/.re_exe.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/.re_exe.eobjs/byte/dune__exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.re_exe.eobjs/byte))))
   (library
    ((name bar)
     (uid 7dc7853647f347467f62c91edc898b00)
     (local true)
     (requires ())
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
    ((name cmdliner)
     (uid a392325be68958876f9598a822187d55)
     (local false)
     (requires ())
     (source_dir /FINDLIB/cmdliner)
     (modules ())
     (include_dirs (/FINDLIB/cmdliner))))
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
     (uid ff1580960f48a57430e95cef762bf472)
     (local true)
     (requires
      (c1f4a4a8fcca6a7045ebee5b639dc729 2c25d00eb24f552a85fba122b2d45dd1))
     (source_dir _build/default)
     (modules
      (((name Dummy_ppx)
        (impl (_build/default/dummy_ppx.ml))
        (intf ())
        (cmt (_build/default/.dummy_ppx.objs/byte/dummy_ppx.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/.dummy_ppx.objs/byte))))
   (library
    ((name foo)
     (uid 536b4af2ace976f46343f6b47c2c32f2)
     (local true)
     (requires (5a9c5cdc441305690411419a4a5e3f34))
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
     (uid 5a9c5cdc441305690411419a4a5e3f34)
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
    ((name per_module_action_exe)
     (uid fa77b53b2ba88e3d4371707df82dbd13)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Per_module_action_exe)
        (impl (_build/default/per_module_action_exe.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl
           (Action3 Action4)))))
       ((name Action4)
        (impl (_build/default/action4.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__Action4.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Action3)
        (impl (_build/default/action3.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__Action3.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Per_module_action_exe__)
        (impl (_build/default/per_module_action_exe__.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_action_exe.objs/byte/per_module_action_exe__.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.per_module_action_exe.objs/byte))))
   (library
    ((name per_module_action_lib)
     (uid 40a11de60c0aa7eb6cbe99a43d4acc3e)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Action2)
        (impl (_build/default/action2.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib__Action2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (Action1)))))
       ((name Action1)
        (impl (_build/default/action1.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib__Action1.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Per_module_action_lib)
        (impl (_build/default/per_module_action_lib.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_action_lib.objs/byte/per_module_action_lib.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.per_module_action_lib.objs/byte))))
   (library
    ((name per_module_pp_lib)
     (uid 5e6b5e06d15bbba69a481ad64dbed48d)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Pp2)
        (impl (_build/default/pp2.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib__Pp2.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Pp1)
        (impl (_build/default/pp1.ml))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib__Pp1.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Per_module_pp_lib)
        (impl (_build/default/per_module_pp_lib.ml-gen))
        (intf ())
        (cmt
         (_build/default/.per_module_pp_lib.objs/byte/per_module_pp_lib.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.per_module_pp_lib.objs/byte))))
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
    ((name re_lib)
     (uid 70105dd95724ac832af7c89df2e2ff65)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Re_lib2)
        (impl (_build/default/re_lib2.re))
        (intf (_build/default/re_lib2.rei))
        (cmt (_build/default/.re_lib.objs/byte/re_lib__Re_lib2.cmt))
        (cmti (_build/default/.re_lib.objs/byte/re_lib__Re_lib2.cmti))
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Re_lib1)
        (impl (_build/default/re_lib1.re))
        (intf ())
        (cmt (_build/default/.re_lib.objs/byte/re_lib__Re_lib1.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Re_lib)
        (impl (_build/default/re_lib.ml-gen))
        (intf ())
        (cmt (_build/default/.re_lib.objs/byte/re_lib.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.re_lib.objs/byte))))
   (library
    ((name sexplib0)
     (uid e7b78a2ef3334358a41a86855c8739c2)
     (local false)
     (requires ())
     (source_dir /FINDLIB/sexplib0)
     (modules ())
     (include_dirs (/FINDLIB/sexplib0))))
   (library
    ((name stdlib-shims)
     (uid dc50e9309dbe56e9ba5a145dd0c7d272)
     (local false)
     (requires ())
     (source_dir /FINDLIB/stdlib-shims)
     (modules ())
     (include_dirs (/FINDLIB/stdlib-shims))))
   (library
    ((name subfolder_lib)
     (uid 3ce0ac7acd90d422db7ffc54ebbdee57)
     (local true)
     (requires ())
     (source_dir _build/default/subdir/subfolder)
     (modules
      (((name Subfolder_lib)
        (impl (_build/default/subdir/subfolder/subfolder_lib.ml))
        (intf ())
        (cmt
         (_build/default/subdir/subfolder/.subfolder_lib.objs/byte/subfolder_lib.cmt))
        (cmti ())
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/subdir/subfolder/.subfolder_lib.objs/byte))))
   (library
    ((name virtual)
     (uid f6cee13b744e81f71621346d08c8df22)
     (local true)
     (requires ())
     (source_dir _build/default/virtual)
     (modules
      (((name Virtual)
        (impl ())
        (intf (_build/default/virtual/virtual.mli))
        (cmt ())
        (cmti (_build/default/virtual/.virtual.objs/byte/virtual.cmti))
        (module_deps ((for_intf ()) (for_impl ()))))))
     (include_dirs (_build/default/virtual/.virtual.objs/byte))))
   (library
    ((name virtual_impl1)
     (uid 9bf1effa3fd6524dd5b76957e6245f85)
     (local true)
     (requires (f6cee13b744e81f71621346d08c8df22))
     (source_dir _build/default/virtual_impl1)
     (modules
      (((name Virtual)
        (impl (_build/default/virtual_impl1/virtual.ml))
        (intf ())
        (cmt
         (_build/default/virtual_impl1/.virtual_impl1.objs/byte/virtual.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Virtual__virtual_impl1__)
        (impl (_build/default/virtual_impl1/virtual__virtual_impl1__.ml-gen))
        (intf ())
        (cmt
         (_build/default/virtual_impl1/.virtual_impl1.objs/byte/virtual__virtual_impl1__.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/virtual_impl1/.virtual_impl1.objs/byte))))
   (library
    ((name virtual_impl2)
     (uid 8fd0490dfc3b56d3874fbbf6eb4abbac)
     (local true)
     (requires (f6cee13b744e81f71621346d08c8df22))
     (source_dir _build/default/virtual_impl2)
     (modules
      (((name Virtual)
        (impl (_build/default/virtual_impl2/virtual.ml))
        (intf ())
        (cmt
         (_build/default/virtual_impl2/.virtual_impl2.objs/byte/virtual.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name Virtual__virtual_impl2__)
        (impl (_build/default/virtual_impl2/virtual__virtual_impl2__.ml-gen))
        (intf ())
        (cmt
         (_build/default/virtual_impl2/.virtual_impl2.objs/byte/virtual__virtual_impl2__.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/virtual_impl2/.virtual_impl2.objs/byte)))))

  $ dune describe workspace --lang 0.1 --sanitize-for-tests virtual
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name virtual)
     (uid f6cee13b744e81f71621346d08c8df22)
     (local true)
     (requires ())
     (source_dir _build/default/virtual)
     (modules
      (((name Virtual)
        (impl ())
        (intf (_build/default/virtual/virtual.mli))
        (cmt ())
        (cmti (_build/default/virtual/.virtual.objs/byte/virtual.cmti)))))
     (include_dirs (_build/default/virtual/.virtual.objs/byte)))))


Test other formats
------------------

  $ dune describe workspace --format csexp --lang 0.1 --sanitize-for-tests | cut -c 1-85
  ((4:root15:/WORKSPACE_ROOT)(13:build_context14:_build/default)(11:executables((5:name

Test errors
-----------

  $ dune describe workspacw --lang 0.1 
  dune: unknown command 'workspacw', did you mean 'workspace'?
  Usage: dune describe [COMMAND] …
  Try 'dune describe --help' or 'dune --help' for more information.
  [1]

  $ dune describe workspace --lang 0.1 xxx
  Error: No such file or directory: xxx
  [1]

  $ touch yyy
  $ dune describe workspace --lang 0.1 yyy
  Error: File exists, but is not a directory: yyy
  [1]

  $ dune describe workspace --lang 1.0
  dune: Only --lang 0.1 is available at the moment as this command is not yet
        stabilised. If you would like to release a software that relies on the output
        of 'dune describe', please open a ticket on
        https://github.com/ocaml/dune.
  Usage: dune describe workspace [OPTION]… [DIRS]…
  Try 'dune describe workspace --help' or 'dune --help' for more information.
  [1]

opam file listing
-----------------

  $ dune describe opam-files --lang 0.1 | dune_cmd expand_lines
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
