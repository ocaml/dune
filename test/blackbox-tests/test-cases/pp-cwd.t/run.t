We test two different aspects of preprocessors:

* The directory in which they run
* How they reference their file dependencies (relative to what?)

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

First, we demonstrate that preprocessors run from the context root:

  $ DIR=1
  $ mkdir $DIR
  $ cat >$DIR/dune <<EOF
  > (test
  >  (name test)
  >  (preprocess
  >   (action (run %{bin:dunepp} %{input-file} $DIR))))
  > EOF
  $ cat >$DIR/test.ml <<EOF
  > print_endline _STRING_
  > EOF
  $ dune runtest $DIR
  running preprocessor in $TESTCASE_ROOT/_build/default
  Hello, world!

While running it from the context root is good for error messages, it makes
referencing processor dependencies quite awkward:

  $ DIR=2
  $ mkdir $DIR
  $ touch $DIR/dep
  $ cat >$DIR/dune <<EOF
  > (test
  >  (name test)
  >  (preprocessor_deps dep)
  >  (preprocess
  >   (action (run %{bin:dunepp} %{input-file} $DIR dep))))
  > EOF
  $ cat >$DIR/test.ml <<EOF
  > print_endline _STRING_
  > EOF
  $ dune runtest $DIR
  running preprocessor in $TESTCASE_ROOT/_build/default
  dep dep exists = false
  dep 2/dep exists = true
  Hello, world!
