Wrapping prefixes module object names, which must be removed for codept.
Otherwise codept will not resolve the right signatures and just overapproximate.

With codept, module A should only depend on B, D and E,
but not C, which is spurious.

  $ dune describe workspace --with-deps --sanitize-for-tests
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name example)
     (uid ef90b2d8192f20023ed204ddd33e6a29)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name E)
        (impl (_build/default/e.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example__E.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name D)
        (impl (_build/default/d.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example__D.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (C)))))
       ((name C)
        (impl (_build/default/c.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example__C.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (B)))))
       ((name B)
        (impl (_build/default/b.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example__B.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name A)
        (impl (_build/default/a.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example__A.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl
           (B D E)))))
       ((name Example)
        (impl (_build/default/example.ml-gen))
        (intf ())
        (cmt (_build/default/.example.objs/byte/example.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))))
     (include_dirs (_build/default/.example.objs/byte)))))
