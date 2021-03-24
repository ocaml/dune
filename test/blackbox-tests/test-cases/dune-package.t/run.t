  $ dune build
  $ dune_cmd cat _build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name a)
  (sections
   (lib
    $TESTCASE_ROOT/_build/install/default/lib/a)
   (libexec
    $TESTCASE_ROOT/_build/install/default/lib/a))
  (files
   (lib
    (META
     a$ext_lib
     a.cma
     a.cmi
     a.cmt
     a.cmx
     a.cmxa
     a.ml
     a__X.cmi
     a__X.cmt
     a__X.cmx
     b/c/.private/c__Y.cmi
     b/c/.private/c__Y.cmt
     b/c/.private/c__Y.cmti
     b/c/c$ext_lib
     b/c/c.cma
     b/c/c.cmi
     b/c/c.cmt
     b/c/c.cmx
     b/c/c.cmxa
     b/c/c.ml
     b/c/c__Y.cmx
     b/c/y.ml
     b/c/y.mli
     byte_only/d.cma
     byte_only/d.cmi
     byte_only/d.cmt
     byte_only/d.ml
     byte_only/d__Z.cmi
     byte_only/d__Z.cmt
     byte_only/z.ml
     dune-package
     opam
     x.ml))
   (libexec (a.cmxs b/c/c.cmxs)))
  (library
   (name a)
   (kind normal)
   (archives (byte a.cma) (native a.cmxa))
   (plugins (byte a.cma) (native a.cmxs))
   (native_archives a$ext_lib)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name A)
     (modules ((name X) (obj_name a__X) (visibility public) (impl)))
     (alias_module
      (name A)
      (obj_name a)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.b.c)
   (kind normal)
   (archives (byte b/c/c.cma) (native b/c/c.cmxa))
   (plugins (byte b/c/c.cma) (native b/c/c.cmxs))
   (native_archives b/c/c$ext_lib)
   (main_module_name C)
   (modes byte native)
   (obj_dir (private_dir .private))
   (modules
    (wrapped
     (main_module_name C)
     (modules ((name Y) (obj_name c__Y) (visibility private) (impl) (intf)))
     (alias_module
      (name C)
      (obj_name c)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.byte_only)
   (kind normal)
   (archives (byte byte_only/d.cma))
   (plugins (byte byte_only/d.cma))
   (main_module_name D)
   (modes byte)
   (modules
    (wrapped
     (main_module_name D)
     (modules ((name Z) (obj_name d__Z) (visibility public) (impl)))
     (alias_module
      (name D)
      (obj_name d)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))

Build with "--store-orig-source-dir" profile
  $ dune build --store-orig-source-dir
  $ dune_cmd cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)

Build with "DUNE_STORE_ORIG_SOURCE_DIR=true" profile
  $ DUNE_STORE_ORIG_SOURCE_DIR=true dune build
  $ dune_cmd cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
