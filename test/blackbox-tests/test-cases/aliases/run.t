  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER build -j1 --root . @just-in-src
  running in src
  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER build -j1 --root . @everywhere
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ $JBUILDER clean -j1 --root .
  $ $JBUILDER build -j1 --root . @x
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ $JBUILDER build -j1 --root . @plop
  File "<command-line>", line 1, characters 0-0:
  Error: This alias is empty.
  Alias "plop" is not defined in . or any of its descendants.
  [1]
  $ $JBUILDER build -j1 --root . @truc/x
  File "<command-line>", line 1, characters 0-0:
  Error: Don't know about directory truc!
  [1]
