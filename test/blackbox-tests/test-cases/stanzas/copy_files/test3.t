Test (alias ...) and (mode ...) fields:

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ cat >dune <<EOF
  > (copy_files
  >  (alias foo)
  >  (mode promote-until-clean)
  >  (files subdir/*.txt))
  > EOF
  $ mkdir -p subdir
  $ echo Foo >subdir/foo.txt

  $ dune build @foo
  File "dune", line 2, characters 1-12:
  2 |  (alias foo)
       ^^^^^^^^^^^
  Error: 'alias' is only available since version 2.7 of the dune language.
  Please update your dune-project file to have (lang dune 2.7).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ dune build @foo

  $ cat foo.txt
  Foo
