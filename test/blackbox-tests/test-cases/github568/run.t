  $ $JBUILDER runtest --display short -j1 -p lib1 --debug-dependency-path
  File "jbuild", line 1, characters 0-464:
  Error: Library "lib2" not found.
  -> required by .merlin
  -> required by .merlin-exists
  -> required by test1.exe
  -> required by alias runtest
  -> required by alias runtest
  Hint: try: jbuilder external-lib-deps --missing -p lib1 @runtest
      ocamldep test1.ml.d
      ocamldep lib1.ml.d
  [1]
