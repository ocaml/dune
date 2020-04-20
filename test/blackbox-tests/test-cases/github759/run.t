  $ dune build foo.cma --profile release
  $ cat _build/default/.merlin-conf
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ rm -f _build/default/.merlin-conf
  $ dune build foo.cma --profile release
  $ cat _build/default/.merlin-conf
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ echo toto > _build/default/.merlin-conf
  $ dune build foo.cma --profile release
  $ cat _build/default/.merlin-conf
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
