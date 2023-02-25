Test dependency on installed package

  $ mkdir -p lib-a lib-a/sub b prefix

  $ cat > lib-a/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > lib-a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (modes melange)
  >  (public_name a))
  > EOF

  $ cat > lib-a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > lib-a/sub/sub.ml <<EOF
  > let y = "bar"
  > EOF

  $ dune build --root lib-a
  Entering directory 'lib-a'
  Leaving directory 'lib-a'

  $ dune install --root lib-a --prefix $PWD/prefix --display short
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
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Sub.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Sub.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Sub.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/sub.ml

  $ cat >b/dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > b/dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (libraries a)
  >  (module_system commonjs))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @dist --display=short
  Entering directory 'b'
          melc node_modules/a/a.js
          melc node_modules/a/foo.js
          melc node_modules/a/sub/sub.js
          melc ...mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc bar.js
  Leaving directory 'b'

  $ find b/_build/default | sort | grep '\.js'
  b/_build/default/bar.js
  b/_build/default/node_modules/a/a.js
  b/_build/default/node_modules/a/foo.js
  b/_build/default/node_modules/a/sub/sub.js
  $ node b/_build/default/bar.js
  foo
