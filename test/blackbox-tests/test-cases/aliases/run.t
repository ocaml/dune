  $ jbuilder clean --display short
  $ jbuilder build --display short @just-in-src
  running in src
  $ jbuilder clean --display short
  $ jbuilder build --display short @everywhere
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ jbuilder clean --display short
  $ jbuilder build --display short @x
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ jbuilder build --display short @plop
  From the command line:
  Error: Alias plop is empty.
  It is not defined in . or any of its descendants.
  [1]
  $ jbuilder build --display short @truc/x
  From the command line:
  Error: Don't know about directory truc!
  [1]
