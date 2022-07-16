  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ FILE=$PWD/main.ml
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((5:ERROR68:No config found for file "main.ml" in ".". Try calling `dune build`.))Bad input: invalid character '\n', expected '(', ')' or '0'..'9'

  $ dune build @check

  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.main.eobjs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Dune__exe?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

  $ FILE=$PWD/lib3.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

If a file has a name of the kind `module_name.xx.xxx.ml/i`
we consider it as ``module_name.ml/i`
This can be useful when some build scripts perform custom
preprocessing and copy files around.
  $ FILE=lib3.foobar.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

If a directory has no configuration the configuration of its parent is used
This can be useful when some build scripts copy files from subdirectories.
  $ FILE=some_sub_dir/lib3.foobar.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

Test of an valid invalid module name
  $ FILE=not-a-module-name.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-w?:-24)))

Dune should also provide configuration when the file is in the build folder
  $ FILE=$PWD/_build/default/lib3.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

  $ FILE=_build/default/lib3.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.mylib.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.mylib3.objs/byte)(?:S?:$TESTCASE_ROOT)(?:FLG(?:-open?:Mylib?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))
