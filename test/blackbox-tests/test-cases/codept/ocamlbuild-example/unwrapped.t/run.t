Create project here, so we have permissions to append below.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

With ``ocamldep``, module A depends on B, C, D, and E,
but C is spurious.

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
        (cmt (_build/default/.example.objs/byte/e.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name D)
        (impl (_build/default/d.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/d.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (C)))))
       ((name C)
        (impl (_build/default/c.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/c.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (B)))))
       ((name B)
        (impl (_build/default/b.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/b.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name A)
        (impl (_build/default/a.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/a.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl
           (B C D E)))))))
     (include_dirs (_build/default/.example.objs/byte)))))

Enable ``codept``.

  $ cat >>dune-project <<EOF
  > (using codept 0.1)
  > EOF

With ``codept``, module A should only depend on B, D, and E,
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
        (cmt (_build/default/.example.objs/byte/e.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name D)
        (impl (_build/default/d.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/d.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (C)))))
       ((name C)
        (impl (_build/default/c.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/c.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl (B)))))
       ((name B)
        (impl (_build/default/b.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/b.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl ()))))
       ((name A)
        (impl (_build/default/a.ml))
        (intf ())
        (cmt (_build/default/.example.objs/byte/a.cmt))
        (cmti ())
        (module_deps
         ((for_intf ())
          (for_impl
           (B D E)))))))
     (include_dirs (_build/default/.example.objs/byte)))))
