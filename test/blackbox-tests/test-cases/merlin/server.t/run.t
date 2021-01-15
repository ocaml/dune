  $ FILE=$PWD/main.ml
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((5:ERROR48:Project isn't built. (Try calling `dune build`.)))

  $ dune build @check

  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.main.eobjs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Dune__exe?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

  $ FILE=$PWD/lib3.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))
