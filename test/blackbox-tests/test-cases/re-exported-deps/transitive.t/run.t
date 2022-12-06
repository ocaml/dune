dependencies can be exported transitively:
  $ dune exec ./foo.exe

transitive deps expressed in the dune-package

  $ dune build @install
  $ dune_cmd cat _build/install/default/lib/pkg/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name pkg)
  (sections (lib .) (libexec .))
  (files
   (lib
    (META
     aaa/aaa$ext_lib
     aaa/aaa.cma
     aaa/aaa.cmi
     aaa/aaa.cmt
     aaa/aaa.cmx
     aaa/aaa.cmxa
     aaa/aaa.ml
     bbb/bbb$ext_lib
     bbb/bbb.cma
     bbb/bbb.cmi
     bbb/bbb.cmt
     bbb/bbb.cmx
     bbb/bbb.cmxa
     bbb/bbb.ml
     ccc/ccc$ext_lib
     ccc/ccc.cma
     ccc/ccc.cmi
     ccc/ccc.cmt
     ccc/ccc.cmx
     ccc/ccc.cmxa
     ccc/ccc.ml
     dune-package))
   (libexec (aaa/aaa.cmxs bbb/bbb.cmxs ccc/ccc.cmxs)))
  (library
   (name pkg.aaa)
   (kind normal)
   (archives (byte aaa/aaa.cma) (native aaa/aaa.cmxa))
   (plugins (byte aaa/aaa.cma) (native aaa/aaa.cmxs))
   (native_archives aaa/aaa$ext_lib)
   (requires pkg.ccc (re_export pkg.bbb))
   (main_module_name Aaa)
   (modes byte native)
   (modules
    (singleton (name Aaa) (obj_name aaa) (path Aaa) (visibility public) (impl))))
  (library
   (name pkg.bbb)
   (kind normal)
   (archives (byte bbb/bbb.cma) (native bbb/bbb.cmxa))
   (plugins (byte bbb/bbb.cma) (native bbb/bbb.cmxs))
   (native_archives bbb/bbb$ext_lib)
   (requires (re_export pkg.ccc))
   (main_module_name Bbb)
   (modes byte native)
   (modules
    (singleton (name Bbb) (obj_name bbb) (path Bbb) (visibility public) (impl))))
  (library
   (name pkg.ccc)
   (kind normal)
   (archives (byte ccc/ccc.cma) (native ccc/ccc.cmxa))
   (plugins (byte ccc/ccc.cma) (native ccc/ccc.cmxs))
   (native_archives ccc/ccc$ext_lib)
   (main_module_name Ccc)
   (modes byte native)
   (modules
    (singleton (name Ccc) (obj_name ccc) (path Ccc) (visibility public) (impl))))
