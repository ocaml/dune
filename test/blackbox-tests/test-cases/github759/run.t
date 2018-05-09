  $ jbuilder build foo.cma
  $ cat .merlin
  B _build/default/.foo.objs
  FLG -open Foo -w -40
  S .
  $ rm -f .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  cat: .merlin: No such file or directory
  [1]
  $ echo toto > .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  toto
