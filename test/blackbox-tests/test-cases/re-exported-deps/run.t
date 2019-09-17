dependencies can be exported transitively:
  $ dune exec ./foo.exe --root transitive
  Entering directory 'transitive'
  Entering directory 'transitive'

transtive deps expressed in the dune-package

  $ dune build @install --root transitive
  Entering directory 'transitive'
  $ cat transitive/_build/install/default/lib/pkg/dune-package
  (lang dune 2.0)
  (name pkg)
  (library
   (name pkg.aaa)
   (kind normal)
   (archives (byte aaa/aaa.cma) (native aaa/aaa.cmxa))
   (plugins (byte aaa/aaa.cma) (native aaa/aaa.cmxs))
   (foreign_archives (native aaa/aaa$ext_lib))
   (requires (re_export pkg.bbb))
   (main_module_name Aaa)
   (modes byte native)
   (modules (singleton (name Aaa) (obj_name aaa) (visibility public) (impl))))
  (library
   (name pkg.bbb)
   (kind normal)
   (archives (byte bbb/bbb.cma) (native bbb/bbb.cmxa))
   (plugins (byte bbb/bbb.cma) (native bbb/bbb.cmxs))
   (foreign_archives (native bbb/bbb$ext_lib))
   (requires (re_export pkg.ccc))
   (main_module_name Bbb)
   (modes byte native)
   (modules (singleton (name Bbb) (obj_name bbb) (visibility public) (impl))))
  (library
   (name pkg.ccc)
   (kind normal)
   (archives (byte ccc/ccc.cma) (native ccc/ccc.cmxa))
   (plugins (byte ccc/ccc.cma) (native ccc/ccc.cmxs))
   (foreign_archives (native ccc/ccc$ext_lib))
   (main_module_name Ccc)
   (modes byte native)
   (modules (singleton (name Ccc) (obj_name ccc) (visibility public) (impl))))

Attempting to re-export dependencies outside of libraries fails:
  $ dune build --root re-export-bad-attempt @all
  Entering directory 're-export-bad-attempt'
