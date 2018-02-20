  $ $JBUILDER exec ./test.exe -j1 --debug-dep --display short --root .
  Multiple rules generated for _build/default/lib.o:
  - <internal location>
  - <internal location>
  [1]

  $ $JBUILDER build src/a.cma -j1 --debug-dep --display short --root .
  Multiple rules generated for _build/default/src/x.ml.d:
  - <internal location>
  - <internal location>
  [1]
