  $ dune exec ./test.exe --debug-dep --display short
  File "dune", line 1, characters 0-0:
  Warning: Module "Lib" is used in several stanzas:
  - dune:6
  - dune:2
  To remove this warning, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this jbuild file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  This warning will become an error in the future.
      ocamldep lib.ml.d
        ocamlc .lib.objs/lib.{cmi,cmo,cmt}
      ocamlopt .lib.objs/lib.{cmx,o}
      ocamlopt lib.{a,cmxa}
      ocamldep test.ml.d
        ocamlc .test.eobjs/lib.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/lib.{cmx,o}
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe
  foo bar

  $ dune build src/a.cma --debug-dep --display short
  File "src/dune", line 1, characters 0-0:
  Warning: Module "X" is used in several stanzas:
  - src/dune:2
  - src/dune:1
  To remove this warning, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this jbuild file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  This warning will become an error in the future.
      ocamldep src/x.ml.d
        ocamlc src/.a.objs/a.{cmi,cmo,cmt}
        ocamlc src/.a.objs/a__X.{cmi,cmo,cmt}
        ocamlc src/a.cma
