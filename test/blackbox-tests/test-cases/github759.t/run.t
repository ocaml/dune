  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))

  $ rm -f .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))

  $ echo toto > .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))
