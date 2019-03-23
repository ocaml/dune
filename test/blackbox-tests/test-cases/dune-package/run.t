  $ dune build
  $ cat _build/install/default/lib/a/dune-package
  (lang dune 1.9)
  (name a)
  (library
   (name a)
   (kind normal)
   (archives (byte a.cma) (native a.cmxa))
   (plugins (byte a.cma) (native a.cmxs))
   (foreign_archives (native a$ext_lib))
   (main_module_name A)
   (modes byte native)
   (modules
    (alias_module (name A) (obj_name a) (visibility public) (impl))
    (main_module_name A)
    (modules ((name X) (obj_name a__X) (visibility public) (impl)))
    (wrapped true)))
  (library
   (name a.b.c)
   (kind normal)
   (archives (byte b/c/c.cma) (native b/c/c.cmxa))
   (plugins (byte b/c/c.cma) (native b/c/c.cmxs))
   (foreign_archives (native b/c/c$ext_lib))
   (main_module_name C)
   (modes byte native)
   (modules
    (alias_module (name C) (obj_name c) (visibility public) (impl))
    (main_module_name C)
    (modules ((name Y) (obj_name c__Y) (visibility private) (impl) (intf)))
    (wrapped true)))

Build with "--store-orig-source-dir" profile
  $ dune build --store-orig-source-dir
  $ cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)

Build with "DUNE_STORE_ORIG_SOURCE_DIR=true" profile
  $ DUNE_STORE_ORIG_SOURCE_DIR=true dune build
  $ cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
