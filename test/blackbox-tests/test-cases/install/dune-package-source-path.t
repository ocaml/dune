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

  $ mkdir -p b/child
  $ cat > b/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name b))
  > EOF
  $ cat > b/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name b)
  >  (public_name b.sub))
  > EOF
  $ cat > b/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > b/child/bar.ml <<EOF
  > let x = "bar"
  > EOF


  $ dune build b.install --root b
  Entering directory 'b'
  Leaving directory 'b'

  $ cat b/_build/default/b.install
  lib: [
    "_build/install/default/lib/b/META"
    "_build/install/default/lib/b/dune-package"
    "_build/install/default/lib/b/sub/b.a" {"sub/b.a"}
    "_build/install/default/lib/b/sub/b.cma" {"sub/b.cma"}
    "_build/install/default/lib/b/sub/b.cmi" {"sub/b.cmi"}
    "_build/install/default/lib/b/sub/b.cmt" {"sub/b.cmt"}
    "_build/install/default/lib/b/sub/b.cmx" {"sub/b.cmx"}
    "_build/install/default/lib/b/sub/b.cmxa" {"sub/b.cmxa"}
    "_build/install/default/lib/b/sub/b.ml" {"sub/b.ml"}
    "_build/install/default/lib/b/sub/b__Child.cmi" {"sub/b__Child.cmi"}
    "_build/install/default/lib/b/sub/b__Child.cmt" {"sub/b__Child.cmt"}
    "_build/install/default/lib/b/sub/b__Child.cmx" {"sub/b__Child.cmx"}
    "_build/install/default/lib/b/sub/b__Child__Bar.cmi" {"sub/b__Child__Bar.cmi"}
    "_build/install/default/lib/b/sub/b__Child__Bar.cmt" {"sub/b__Child__Bar.cmt"}
    "_build/install/default/lib/b/sub/b__Child__Bar.cmx" {"sub/b__Child__Bar.cmx"}
    "_build/install/default/lib/b/sub/b__Foo.cmi" {"sub/b__Foo.cmi"}
    "_build/install/default/lib/b/sub/b__Foo.cmt" {"sub/b__Foo.cmt"}
    "_build/install/default/lib/b/sub/b__Foo.cmx" {"sub/b__Foo.cmx"}
    "_build/install/default/lib/b/sub/child/bar.ml" {"sub/child/bar.ml"}
    "_build/install/default/lib/b/sub/child/child.ml" {"sub/child/child.ml"}
    "_build/install/default/lib/b/sub/foo.ml" {"sub/foo.ml"}
  ]
  libexec: [
    "_build/install/default/lib/b/sub/b.cmxs" {"sub/b.cmxs"}
  ]

  $ cat b/_build/install/default/lib/b/dune-package | grep path
       (source (path B) (impl (path sub/b.ml-gen))))
         (source (path Child Child) (impl (path sub/b__Child.ml-gen))))
          (source (path Child Bar) (impl (path sub/child/bar.ml))))))
        (source (path Foo) (impl (path sub/foo.ml))))))

