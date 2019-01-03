  $ jbuilder build foo.cma
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ rm -f .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ echo toto > .merlin
  $ jbuilder build foo.cma
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
