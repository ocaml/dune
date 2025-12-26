This test is guarded by ocaml version >= 5.2, so it should include foo with -H when
implicit_transitive_deps is set to false.

  $ jqScript=$(mktemp)
  $ cat >$jqScript <<'EOF'
  > .[] |
  > .args.process_args |
  > select(. != null) |
  > select(index("run.ml")) as $arr |
  > [range(0; $arr | length - 1) as $i |
  >   if $arr[$i] == "-I" or $arr[$i] == "-H" then [$arr[$i], $arr[$i + 1]] else empty end]
  > | .[]
  > EOF

  $ getincludes () {
  > dune build ./run.exe
  > dune trace cat | jq -c -f $jqScript
  > }

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps true)
  > EOF

  $ getincludes
  ["-I",".run.eobjs/byte"]
  ["-I",".run.eobjs/native"]
  ["-I",".bar.objs/byte"]
  ["-I",".bar.objs/native"]
  ["-I",".foo.objs/byte"]
  ["-I",".foo.objs/native"]

In the following two tests we use "false-if-hidden-includes-supported" for
testing purposes, but since this test is guarded by OCaml version >= 5.2, this
should be equivalent to "false".

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ getincludes
  ["-I",".run.eobjs/byte"]
  ["-I",".run.eobjs/native"]
  ["-I",".bar.objs/byte"]
  ["-I",".bar.objs/native"]
  ["-H",".foo.objs/byte"]
  ["-H",".foo.objs/native"]

Test transitive deps can not be directly accessed, both for compiler versions supporting -H or not:

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ dune build ./runf.exe 2>&1 | grep -v ocamlc
  File "runf.ml", line 1, characters 16-19:
  1 | let _ = Bar.y + Foo.v
                      ^^^
  Error: Unbound module Foo
