We demonstrate that users are allowed to generate rules in "private" dune
directories. That should be forbidden or dune should generate this stuff
elsewhere.

  $ cat >dune-project <<EOF
  > (lang dune 3.1)
  > EOF

  $ cat >dune <<EOF
  > (dirs :standard .foo.eobjs)
  > (subdir .foo.eobjs
  >  (rule (with-stdout-to foo (echo "foo"))))
  > EOF

  $ target=".foo.eobjs/foo"
  $ dune build $target
  $ cat _build/default/$target
  foo

  $ cat >>dune <<EOF
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > print_endline "42";;
  > EOF

  $ dune build ./foo.exe
  File "dune", line 5, characters 7-10:
  5 |  (name foo))
             ^^^
  Error: No rule found for .foo.eobjs/native/dune__exe__Foo.cmx
  File "dune", line 5, characters 7-10:
  5 |  (name foo))
             ^^^
  Error: No rule found for .foo.eobjs/native/dune__exe__Foo.o
  [1]
