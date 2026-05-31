Test dependency on installed package

  $ mkdir a b prefix app

  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
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

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/foo.ml

  $ cat >b/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name b))
  > (using melange 0.1)
  > EOF

  $ cat > b/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name b)
  >  (libraries a))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = A.Foo.x
  > EOF

  $ cat > b/foo.ml <<EOF
  > let x = Bar.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @install
  $ find b/_build/default/.b.objs/melange -type f | sort
  b/_build/default/.b.objs/melange/b.cmi
  b/_build/default/.b.objs/melange/b.cmj
  b/_build/default/.b.objs/melange/b.cmt
  b/_build/default/.b.objs/melange/b__Bar.cmi
  b/_build/default/.b.objs/melange/b__Bar.cmj
  b/_build/default/.b.objs/melange/b__Bar.cmt
  b/_build/default/.b.objs/melange/b__Foo.cmi
  b/_build/default/.b.objs/melange/b__Foo.cmj
  b/_build/default/.b.objs/melange/b__Foo.cmt

  $ dune install --root b --prefix $PWD/prefix --display=short
  Installing $TESTCASE_ROOT/prefix/lib/b/META
  Installing $TESTCASE_ROOT/prefix/lib/b/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/bar.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/foo.ml

  $ cat >app/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name app))
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias melange-dist)
  >  (emit_stdlib false)
  >  (libraries b))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @melange-dist
  $ find app/_build/default/dist -type f | sort
  app/_build/default/dist/node_modules/a/a.js
  app/_build/default/dist/node_modules/a/foo.js
  app/_build/default/dist/node_modules/b/b.js
  app/_build/default/dist/node_modules/b/bar.js
  app/_build/default/dist/node_modules/b/foo.js
