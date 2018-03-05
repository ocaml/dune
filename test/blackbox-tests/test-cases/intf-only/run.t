Successes:

  $ $JBUILDER build --display short --root foo -j1 --debug-dep 2>&1 | grep -v Entering
      ocamldep test/bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo__.{cmi,cmo,cmt}
      ocamlopt .foo.objs/foo__.{cmx,o}
      ocamldep intf.mli.d
        ocamlc .foo.objs/foo__Intf.{cmi,cmti}
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
        ocamlc test/.bar.objs/bar.{cmi,cmo,cmt}
      ocamlopt test/.bar.objs/bar.{cmx,o}
      ocamlopt test/bar.{a,cmxa}
      ocamlopt test/bar.cmxs
        ocamlc foo.cma
        ocamlc test/bar.cma

Errors:

  $ $JBUILDER build --display short --root a -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 2, characters 1-13:
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation (x y))
  
  This will become an error in the future.
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ $JBUILDER build --display short --root b -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 34-37:
  Warning: The following modules must be listed here as they don't have an implementation:
  - y
  This will become an error in the future.
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ $JBUILDER build --display short --root c -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-58:
  Error: Module X doesn't exist.
  $ $JBUILDER build --display short --root d -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-58:
  Error: Module X has an implementation, it cannot be listed here
