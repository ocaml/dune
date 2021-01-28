dependencies can be exported transitively:
  $ dune exec ./foo.exe --root transitive
  Entering directory 'transitive'
  Entering directory 'transitive'

transtive deps expressed in the dune-package

  $ dune build @install --root transitive
  Entering directory 'transitive'
  $ dune_cmd cat transitive/_build/install/default/lib/pkg/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name pkg)
  (sections
   (lib
    $TESTCASE_ROOT/transitive/_build/install/default/lib/pkg)
   (libexec
    $TESTCASE_ROOT/transitive/_build/install/default/lib/pkg))
  (files
   (lib
    (META
     dune-package
     aaa/aaa.ml
     aaa/aaa.cmi
     aaa/aaa.cmx
     aaa/aaa.cmt
     aaa/aaa.cma
     aaa/aaa.cmxa
     aaa/aaa$ext_lib
     bbb/bbb.ml
     bbb/bbb.cmi
     bbb/bbb.cmx
     bbb/bbb.cmt
     bbb/bbb.cma
     bbb/bbb.cmxa
     bbb/bbb$ext_lib
     ccc/ccc.ml
     ccc/ccc.cmi
     ccc/ccc.cmx
     ccc/ccc.cmt
     ccc/ccc.cma
     ccc/ccc.cmxa
     ccc/ccc$ext_lib))
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
   (modules (singleton (name Aaa) (obj_name aaa) (visibility public) (impl))))
  (library
   (name pkg.bbb)
   (kind normal)
   (archives (byte bbb/bbb.cma) (native bbb/bbb.cmxa))
   (plugins (byte bbb/bbb.cma) (native bbb/bbb.cmxs))
   (native_archives bbb/bbb$ext_lib)
   (requires (re_export pkg.ccc))
   (main_module_name Bbb)
   (modes byte native)
   (modules (singleton (name Bbb) (obj_name bbb) (visibility public) (impl))))
  (library
   (name pkg.ccc)
   (kind normal)
   (archives (byte ccc/ccc.cma) (native ccc/ccc.cmxa))
   (plugins (byte ccc/ccc.cma) (native ccc/ccc.cmxs))
   (native_archives ccc/ccc$ext_lib)
   (main_module_name Ccc)
   (modes byte native)
   (modules (singleton (name Ccc) (obj_name ccc) (visibility public) (impl))))

Re-exporting deps in executables isn't allowed
  $ dune build --root re-export-exe @all
  Entering directory 're-export-exe'
  File "dune", line 7, characters 12-27:
  7 |  (libraries (re_export foo)))
                  ^^^^^^^^^^^^^^^
  Error: re_export is not allowed here
  [1]
