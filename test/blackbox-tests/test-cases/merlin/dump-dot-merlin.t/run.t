  $ dune build
  $ dune ocaml dump-dot-merlin |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  EXCLUDE_QUERY_DIR
  STDLIB OPAM_PREFIX/lib/ocaml
  B $TESTCASE_ROOT/_build/default/.foo.eobjs/byte
  S $TESTCASE_ROOT
  # FLG -pp $TESTCASE_ROOT/_build/default/pp/pp.exe
  # FLG -open Dune__exe -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
  $ dune ocaml dump-dot-merlin "pp" |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  EXCLUDE_QUERY_DIR
  STDLIB OPAM_PREFIX/lib/ocaml
  B $TESTCASE_ROOT/_build/default/pp/.pp.eobjs/byte
  S $TESTCASE_ROOT/pp
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
