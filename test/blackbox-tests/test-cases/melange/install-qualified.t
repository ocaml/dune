Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (modes melange)
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > let bar = Sub.Bar.x
  > EOF

  $ mkdir -p a/sub
  $ cat > a/sub/bar.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub__Bar.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub__Bar.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/a__Sub__Bar.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/sub/bar.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/melange/sub/sub.ml

  $ cat prefix/lib/a/dune-package | grep path
       (source (path A) (impl (path sub/a.ml-gen))))
        (source (path Foo) (impl (path sub/foo.ml))))
         (source (path Sub Sub) (impl (path sub/a__Sub.ml-gen))))
          (source (path Sub Bar) (impl (path sub/sub/bar.ml))))))))

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

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist

  $ find app/_build/default/dist/node_modules/ -type f | sort
  app/_build/default/dist/node_modules/a.sub/a.js
  app/_build/default/dist/node_modules/a.sub/a__Sub.js
  app/_build/default/dist/node_modules/a.sub/foo.js
  app/_build/default/dist/node_modules/a.sub/sub/bar.js
