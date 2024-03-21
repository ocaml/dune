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
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix --display short
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

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @install --display=short
  Entering directory 'b'
      ocamldep .b.objs/b__Bar.impl.d
      ocamldep .b.objs/b__Foo.impl.d
          melc .b.objs/melange/b.{cmi,cmj,cmt}
          melc .b.objs/melange/b__Bar.{cmi,cmj,cmt}
          melc .b.objs/melange/b__Foo.{cmi,cmj,cmt}
  Leaving directory 'b'

  $ dune install --root b --prefix $PWD/prefix --display=short
  Installing $TESTCASE_ROOT/prefix/lib/b/META
  Installing $TESTCASE_ROOT/prefix/lib/b/b.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/bar.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/b/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmt

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

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @melange-dist --display=short
  Entering directory 'app'
          melc dist/node_modules/a/a.js
          melc dist/node_modules/a/foo.js
          melc dist/node_modules/b/b.js
          melc dist/node_modules/b/bar.js
          melc dist/node_modules/b/foo.js
          melc .dist.mobjs/melange/melange.{cmi,cmj,cmt}
          melc dist/.dist.mobjs/melange.js
  Leaving directory 'app'
