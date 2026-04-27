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

It should also be possible to use an installed library whose qualified
subdirectories were renamed.

  $ mkdir lib app prefix
  $ cat >lib/dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name renamed))
  > EOF
  $ cat >lib/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (name renamed)
  >  (public_name renamed))
  > EOF
  $ mkdir lib/internal
  $ cat >lib/internal/leaf.ml <<EOF
  > let value = "from renamed installed dir"
  > EOF

  $ dune build --root lib @install
  $ dune install --root lib --prefix $PWD/prefix --display quiet
  $ cat prefix/lib/renamed/dune-package | grep "path Public Leaf"
          (source (path Public Leaf) (impl (path internal/leaf.ml))))))))

  $ cat >app/dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat >app/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries renamed))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () = print_endline Renamed.Public.Leaf.value
  > EOF
  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune exec --root app ./main.exe
  from renamed installed dir
