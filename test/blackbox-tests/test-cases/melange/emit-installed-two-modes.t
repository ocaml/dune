Test dependency on installed package

  $ mkdir a b c prefix

  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes byte melange)
  >  (public_name a))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmt

  $ cat >b/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @dist --display=short 2>&1 | grep -v melange
  Entering directory 'b'
          melc dist/node_modules/a/a.js
          melc dist/node_modules/a/foo.js
          melc dist/bar.js
  Leaving directory 'b'

  $ node b/_build/default/dist/bar.js
  foo

  $ cat >c/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > c/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a))
  > EOF

  $ cat > c/dune <<EOF
  > (executable
  >  (name bar)
  >  (modes byte)
  >  (libraries a))
  > EOF

  $ cat > c/bar.ml <<EOF
  > let () = print_endline A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune exec ./c/bar.exe
  foo
