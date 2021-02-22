  $ dune build
  $ dune ocaml dump-dot-merlin src\ with\ spaces |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  EXCLUDE_QUERY_DIR
  STDLIB OPAM_PREFIX/lib/ocaml
  B $TESTCASE_ROOT/_build/default/src with spaces/.foo.eobjs/byte
  S $TESTCASE_ROOT/src with spaces
  # FLG -pp ''\''$TESTCASE_ROOT/_build/default/p p/pp.exe'\'''
  # FLG -open Dune__exe -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
  $ dune ocaml dump-dot-merlin "p p" |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  EXCLUDE_QUERY_DIR
  STDLIB OPAM_PREFIX/lib/ocaml
  B $TESTCASE_ROOT/_build/default/p p/.pp.eobjs/byte
  S $TESTCASE_ROOT/p p
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
