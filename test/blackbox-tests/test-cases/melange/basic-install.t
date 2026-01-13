Test that we can install melange mode libraries

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build @install
  $ cat ./_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/melange/foo.ml" {"melange/foo.ml"}
  ]

  $ sed -E 's/lang dune [0-9.]+/lang dune XXX/' _build/install/default/lib/foo/dune-package
  (lang dune XXX)
  (name foo)
  (sections (lib .))
  (files
   (lib
    (META
     dune-package
     melange/foo.cmi
     melange/foo.cmj
     melange/foo.cmt
     melange/foo.ml)))
  (library
   (name foo)
   (kind normal)
   (main_module_name Foo)
   (modes melange)
   (modules
    (singleton
     (obj_name foo)
     (visibility public)
     (source (path Foo) (impl (path foo.ml))))))

  $ dune install --prefix prefix --display short
  Installing prefix/lib/foo/META
  Installing prefix/lib/foo/dune-package
  Installing prefix/lib/foo/melange/foo.cmi
  Installing prefix/lib/foo/melange/foo.cmj
  Installing prefix/lib/foo/melange/foo.cmt
  Installing prefix/lib/foo/melange/foo.ml

