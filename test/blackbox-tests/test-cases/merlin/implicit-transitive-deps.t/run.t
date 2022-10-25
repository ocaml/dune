  $ stdlib="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/STDLIB=$stdlib:$BUILD_PATH_PREFIX_MAP"

  $ dune build @check

FIXME: Merlin must be able to locate the definitions of values coming from an
implicit transitive dependency, even when `implicit-transitive-dependency` is
set to `false`. They should be part of the source path returned by the
configuration.

In this test the dependencies are as follow:
main -> lib1 -> lib2 -> stdlib

  $ FILE=$PWD/bin/main.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep ":S?"
  ?:S?:/STDLIB)
  ?:S?:$TESTCASE_ROOT/bin)
  ?:S?:$TESTCASE_ROOT/src/lib1)
  ?:S?:$TESTCASE_ROOT/src/lib2)

  $ FILE=$PWD/src/lib1/lib1.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep ":S?"
  ?:S?:/STDLIB)
  ?:S?:$TESTCASE_ROOT/src/lib1)
  ?:S?:$TESTCASE_ROOT/src/lib2)

  $ FILE=$PWD/src/lib2/lib2.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep ":S?"
  ?:S?:/STDLIB)
  ?:S?:$TESTCASE_ROOT/src/lib2)
