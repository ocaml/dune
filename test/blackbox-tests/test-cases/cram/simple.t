Test of the cram test framework itself

Multiline commands

  $ cat <<EOF
  > Multiline
  > Text
  > EOF
  Multiline
  Text

The environment is preserved across phrases

  $ TOTO=hello
  $ echo $TOTO
  hello

  $ mkdir -p toto
  $ cd toto
  $ pwd
  $TESTCASE_ROOT/toto
  $ cd ..

Printing stuff with backslashes

  $ cat <<EOF
  > abc \ def \n hij
  > EOF
  abc \ def \n hij
