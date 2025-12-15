The presence of an (include_subdirs unqualified) nested under an
(include_subdirs qualified) should work smoothly.

  $ dune exec --display short ./foo.exe
      ocamldep .foo.eobjs/dune__exe__Foo.impl.d
      ocamldep .foo.eobjs/dune__exe__Qux__Quux.impl.d
        ocamlc .foo.eobjs/byte/dune__exe.{cmi,cmo,cmt}
        ocamlc .foo.eobjs/byte/dune__exe__Qux.{cmi,cmo,cmt}
      ocamldep .foo.eobjs/dune__exe__Foo.intf.d
      ocamlopt .foo.eobjs/native/dune__exe.{cmx,o}
        ocamlc .foo.eobjs/byte/dune__exe__Qux__Quux.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/dune__exe__Qux.{cmx,o}
        ocamlc .foo.eobjs/byte/dune__exe__Foo.{cmi,cmti}
      ocamlopt .foo.eobjs/native/dune__exe__Qux__Quux.{cmx,o}
      ocamlopt .foo.eobjs/native/dune__exe__Foo.{cmx,o} (exit 2)
  File "foo.ml", line 3, characters 4-7:
  3 |     Bar.Baaz.baaaz
          ^^^
  Error: Unbound module Bar
  [1]

The correct output should be:

baaaz baaazo beeez quuux

cf https://github.com/ocaml/dune/issues/7630
