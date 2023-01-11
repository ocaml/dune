Test dependency on installed package

  $ mkdir a b prefix

  $ cat > a/dune-project <<EOF
  > (lang dune 3.6)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name a))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmt

  $ cat >b/dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (libraries a)
  >  (module_system commonjs))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @dist --display=short
  Entering directory 'b'
          melc dist/node_modules/a/a.js
          melc dist/node_modules/a/foo.js
          melc .dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc dist/bar.js
  Leaving directory 'b'

  $ node b/_build/default/dist/bar.js
  foo
