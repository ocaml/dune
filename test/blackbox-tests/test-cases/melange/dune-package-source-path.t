Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange)
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmt

  $ cat prefix/lib/a/dune-package | grep path
       (source (path A) (impl (path sub/a.ml-gen))))
        (source (path Foo) (impl (path sub/foo.ml))))))

  $ cat >app/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries a.sub))
  > EOF

  $ cat > app/bar.ml <<EOF
  > let x = Js.log A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist --display short 2>&1 | grep -v melange
  Entering directory 'app'
          melc dist/node_modules/a.sub/a.js
          melc dist/node_modules/a.sub/foo.js
          melc dist/bar.js
  Leaving directory 'app'


  $ ls app/_build/default/dist/node_modules/a.sub
  a.js
  foo.js
