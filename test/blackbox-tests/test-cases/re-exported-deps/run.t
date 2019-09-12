dependencies can be exported transitively:
  $ dune exec ./foo.exe --root transitive
  Entering directory 'transitive'
  Entering directory 'transitive'
        ocamlc .foo.eobjs/byte/dune__exe__Foo.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.08.0/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I .foo.eobjs/byte -I .aaa.objs/byte -I .aaa.objs/native -no-alias-deps -opaque -o .foo.eobjs/byte/dune__exe__Foo.cmo -c -impl foo.ml)
  File "foo.ml", line 2, characters 11-14:
  2 | module B = Bbb
                 ^^^
  Error (warning 49): no cmi file was found in path for module Bbb
  File "foo.ml", line 3, characters 11-14:
  3 | module C = Ccc
                 ^^^
  Error (warning 49): no cmi file was found in path for module Ccc
  [1]

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
   (requires pkg.bbb)
   (main_module_name Aaa)
   (modes byte native)
   (modules (singleton (name Aaa) (obj_name aaa) (visibility public) (impl)))
   (re_exports bbb))
  (library
   (name pkg.bbb)
   (kind normal)
   (archives (byte bbb/bbb.cma) (native bbb/bbb.cmxa))
   (plugins (byte bbb/bbb.cma) (native bbb/bbb.cmxs))
   (foreign_archives (native bbb/bbb$ext_lib))
   (requires pkg.ccc)
   (main_module_name Bbb)
   (modes byte native)
   (modules (singleton (name Bbb) (obj_name bbb) (visibility public) (impl)))
   (re_exports ccc))
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
