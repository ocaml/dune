  $ dune exec ./test.exe --debug-dep --display short --root jbuild --profile release
  Entering directory 'jbuild'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "src/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild", line 1, characters 0-0:
  Warning: Module "Lib" is used in several stanzas:
  - jbuild:2
  - jbuild:6
  To remove this warning, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this jbuild file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  Entering directory 'jbuild'
        ocamlc .test.eobjs/byte/dune__exe.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/native/dune__exe.{cmx,o}
      ocamldep .lib.objs/lib.ml.d
        ocamlc .lib.objs/byte/lib.{cmi,cmo,cmt}
      ocamlopt .lib.objs/native/lib.{cmx,o}
      ocamlopt lib.{a,cmxa}
      ocamldep .test.eobjs/lib.ml.d
        ocamlc .test.eobjs/byte/dune__exe__Lib.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/native/dune__exe__Lib.{cmx,o}
      ocamldep .test.eobjs/test.ml.d
        ocamlc .test.eobjs/byte/dune__exe__Test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/native/dune__exe__Test.{cmx,o}
      ocamlopt test.exe
  foo bar

  $ dune build src/a.cma --debug-dep --display short --root jbuild
  Entering directory 'jbuild'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "src/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "src/jbuild", line 1, characters 0-0:
  Warning: Module "X" is used in several stanzas:
  - src/jbuild:1
  - src/jbuild:2
  To remove this warning, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this jbuild file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
        ocamlc src/.a.objs/byte/a.{cmi,cmo,cmt}
      ocamldep src/.a.objs/x.ml.d
        ocamlc src/.a.objs/byte/a__X.{cmi,cmo,cmt}
        ocamlc src/a.cma

  $ dune exec ./test.exe --debug-dep --display short --root dune
  Entering directory 'dune'
  File "dune", line 1, characters 0-0:
  Error: Module "Lib" is used in several stanzas:
  - dune:1
  - dune:5
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

  $ dune build src/a.cma --debug-dep --display short --root dune
  Entering directory 'dune'
  File "src/dune", line 1, characters 0-0:
  Error: Module "X" is used in several stanzas:
  - src/dune:1
  - src/dune:2
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]
