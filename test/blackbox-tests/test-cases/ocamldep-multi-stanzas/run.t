  $ $JBUILDER exec ./test.exe -j1 --debug-dep --display short --root .
  File "jbuild", line 1, characters 0-0:
  Warning: Module "Lib" is used in several stanzas:
  - jbuild:8
  - jbuild:4
  This will become an error in the future.
  Multiple rules generated for _build/default/lib.o:
  - <internal location>
  - <internal location>
  [1]

  $ $JBUILDER build src/a.cma -j1 --debug-dep --display short --root .
  File "src/jbuild", line 1, characters 0-0:
  Warning: Module "X" is used in several stanzas:
  - src/jbuild:4
  - src/jbuild:3
  This will become an error in the future.
      ocamldep src/x.ml.d
        ocamlc src/.a.objs/a.{cmi,cmo,cmt}
        ocamlc src/.a.objs/a__X.{cmi,cmo,cmt}
        ocamlc src/a.cma
