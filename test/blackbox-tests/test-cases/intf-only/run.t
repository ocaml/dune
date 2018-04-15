Successes:

  $ jbuilder build --display short --root foo --debug-dep 2>&1 | grep -v Entering
      ocamldep test/bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo__.{cmi,cmo,cmt}
      ocamlopt .foo.objs/foo__.{cmx,o}
      ocamldep intf.mli.d
        ocamlc .foo.objs/foo__Intf.{cmi,cmti}
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc test/.bar.objs/bar.{cmi,cmo,cmt}
        ocamlc test/bar.cma
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt test/.bar.objs/bar.{cmx,o}
      ocamlopt test/bar.{a,cmxa}
      ocamlopt test/bar.cmxs
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs

Errors:

  $ jbuilder build --display short --root a foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 2, characters 1-13:
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation (x y))
  
  This will become an error in the future.
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ jbuilder build --display short --root b foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 34-37:
  Warning: The following modules must be listed here as they don't have an implementation:
  - y
  This will become an error in the future.
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ jbuilder build --display short --root c foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 35-36:
  Error: Module X doesn't exist.
  $ jbuilder build --display short --root d foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 35-36:
  Error: Module X has an implementation, it cannot be listed here
