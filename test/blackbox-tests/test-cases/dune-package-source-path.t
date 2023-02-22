Test paths on public libraries with `.` are correct

  $ mkdir a

  $ cat > a/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.a
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/sub/a.cmxs

  $ cat prefix/lib/a/dune-package | grep path
       (source (path A) (impl (path sub/a.ml-gen))))
        (source (path Foo) (impl (path sub/foo.ml))))))
