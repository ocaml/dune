Create project here, so we have permissions to append below.

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > EOF

With ocamldep, module A depends on B, which is spurious.

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
      (((name B)
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
          (for_impl (B)))))))
     (include_dirs (_build/default/.example.objs/byte)))))

Enable codept.

  $ cat >>dune-project <<EOF
  > (using codept 0.1)
  > EOF

With codept, module A should not depend on B, which is spurious.

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
      (((name B)
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
          (for_impl ()))))))
     (include_dirs (_build/default/.example.objs/byte)))))
