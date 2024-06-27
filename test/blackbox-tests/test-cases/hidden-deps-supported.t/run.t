This test is guarded by ocaml version >= 5.2, so it should include foo with -H when
implicit_transitive_deps is set to false.

  $ getincludes () {
  > dune build --verbose ./run.exe 2>&1 | grep run.ml | grep -E -o "\-$1\s(.foo)\S*" | sed s/\-$1//g | tr -d '[:space:]'
  > }

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps true)
  > EOF
  $ echo "$(getincludes I)"
  .foo.objs/byte.foo.objs/byte.foo.objs/native


  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false)
  > EOF

  $ echo "$(getincludes H)"
  .foo.objs/byte.foo.objs/byte.foo.objs/native



Test transitive deps can not be directly accessed, both for compiler versions supporting -H or not:
  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false)
  > EOF

  $ dune build ./runf.exe 2>&1 | grep -v ocamlc
  File "runf.ml", line 1, characters 16-21:
  1 | let a = Bar.y + Foo.v
                      ^^^^^
  Error: Unbound module Foo

Test if the issue #274 is solved: the tyxml.functor is included with -H flag in
ITD = false case, and thus no type abstraction happens when it is used.

  $ dune build --root=./tyxml
  Entering directory 'tyxml'
  Leaving directory 'tyxml'


