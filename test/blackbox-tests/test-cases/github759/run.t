  $ jbuilder build foo.cma
  $ cat .merlin
  B _build/default/.foo.objs
  FLG -open Foo -w -40
  S .
  $ rm -f .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  B _build/default/.foo.objs
  FLG -open Foo -w -40
  S .
  $ echo toto > .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  B _build/default/.foo.objs
  FLG -open Foo -w -40
  S .
