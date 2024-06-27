This test is guarded by ocaml version <= 5.1, so it should not include foo
when implicit_transitive_deps is set to false, i.e. testing backward compatibility of
the new -H feature added.

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

  $ echo "$(getincludes I)"
  
