Should include Foo with -H:

  $ getincludes () {
  > dune build --verbose ./run.exe 2>&1 | grep run.ml | grep -E -o "\-$1\s(.foo)\S*" | sed s/\-$1//g | tr -d '[:space:]'
  > }

  $ supports_H=$(if ocamlc -H x --help >/dev/null 2>&1; then echo true; else echo false; fi)
  $ fooincludes='.foo.objs/byte.foo.objs/byte.foo.objs/native'
  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps true)
  > EOF

  $ if $supports_H; then [ "$(getincludes I)" = $fooincludes ]; fi
  $ if ! $supports_H; then [ "$(getincludes I)" = $fooincludes ]; fi

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false)
  > EOF

  $ if $supports_H; then [ "$(getincludes H)" = $fooincludes ]; fi
  $ if ! $supports_H; then [ "$(getincludes I)" = '' ]; fi


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
