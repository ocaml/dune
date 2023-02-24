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

  $ dune build a.install --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ cat a/_build/default/a.install
  lib: [
    "_build/install/default/lib/a/META"
    "_build/install/default/lib/a/dune-package"
    "_build/install/default/lib/a/sub/a.a" {"sub/a.a"}
    "_build/install/default/lib/a/sub/a.cma" {"sub/a.cma"}
    "_build/install/default/lib/a/sub/a.cmi" {"sub/a.cmi"}
    "_build/install/default/lib/a/sub/a.cmt" {"sub/a.cmt"}
    "_build/install/default/lib/a/sub/a.cmx" {"sub/a.cmx"}
    "_build/install/default/lib/a/sub/a.cmxa" {"sub/a.cmxa"}
    "_build/install/default/lib/a/sub/a.ml" {"sub/a.ml"}
    "_build/install/default/lib/a/sub/a__Foo.cmi" {"sub/a__Foo.cmi"}
    "_build/install/default/lib/a/sub/a__Foo.cmt" {"sub/a__Foo.cmt"}
    "_build/install/default/lib/a/sub/a__Foo.cmx" {"sub/a__Foo.cmx"}
    "_build/install/default/lib/a/sub/foo.ml" {"sub/foo.ml"}
  ]
  libexec: [
    "_build/install/default/lib/a/sub/a.cmxs" {"sub/a.cmxs"}
  ]

  $ cat a/_build/install/default/lib/a/dune-package | grep path
       (source (path A) (impl (path sub/a.ml-gen))))
        (source (path Foo) (impl (path sub/foo.ml))))))
