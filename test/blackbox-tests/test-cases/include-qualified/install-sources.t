It should be possible to install sources with the same file name when
(include_subirs qualified) is used

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package
  >  (name foo))
  > EOF
  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (public_name foo))
  > EOF

First we test the case without any sources. To make sure we can at least
install empty libraries.

  $ dune build foo.install

Now we add some source with duplicate base names and test again:

  $ mkdir bar
  $ touch baz.ml bar/baz.ml
  $ dune build foo.install
  $ cat _build/default/foo.install | grep .ml
    "_build/install/default/lib/foo/bar/bar.ml" {"bar/bar.ml"}
    "_build/install/default/lib/foo/bar/baz.ml" {"bar/baz.ml"}
    "_build/install/default/lib/foo/baz.ml"
    "_build/install/default/lib/foo/foo.ml"

  $ cat _build/install/default/lib/foo/dune-package | grep ".ml"
     bar/bar.ml
     bar/baz.ml
     baz.ml
     foo.ml
       (source (path Foo) (impl (path foo.ml-gen))))
         (source (path Bar Bar) (impl (path foo__Bar.ml-gen))))
          (source (path Bar Baz) (impl (path bar/baz.ml))))))
        (source (path Baz) (impl (path baz.ml))))))
