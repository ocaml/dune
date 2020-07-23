Test that (copy_files ...) works

  $ dune build --root test1 test.exe .merlin
  Entering directory 'test1'
  $ dune build --root test1 @bar-source
  Entering directory 'test1'
  #line 1 "include/bar.h"
  int foo () {return 42;}
  $ dune build --root test2 @foo/cat
  Entering directory 'test2'
  # 1 "dummy.txt"
  hello

Test (alias ...) and (mode ...) fields:

  $ mkdir -p test3
  $ cat >test3/dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ cat >test3/dune <<EOF
  > (copy_files
  >  (alias foo)
  >  (mode promote-until-clean)
  >  (files subdir/*.txt))
  > EOF
  $ mkdir -p test3/subdir
  $ echo Foo >test3/subdir/foo.txt

  $ dune build --root test3 @foo
  Entering directory 'test3'
  File "dune", line 2, characters 1-12:
  2 |  (alias foo)
       ^^^^^^^^^^^
  Error: 'alias' is only available since version 2.7 of the dune language.
  Please update your dune-project file to have (lang dune 2.7).
  [1]

  $ cat >test3/dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ dune build --root test3 @foo
  Entering directory 'test3'

  $ cat test3/foo.txt
  Foo

Test external paths:

  $ mkdir -p test4
  $ cat >test4/dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ P=$(mktemp)
  $ echo Hola > $P
  $ cat >test4/dune <<EOF
  > (copy_files $P)
  > EOF
  $ dune build --root test4 $(basename $P)
  Entering directory 'test4'
  $ cat test4/_build/default/$(basename $P)
  Hola
