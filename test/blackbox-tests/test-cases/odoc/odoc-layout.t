Building @doc and @doc-private produces documentation for package pages,
public libraries, and private libraries with or without a package. Library
.odoc files go in their object directories; page .odoc files, .odocl files,
and HTML go under _doc.

  $ make_dune_project_with_package 3.25 my_pkg
  $ mkdir foo bar priv solo
  $ echo '(documentation (package my_pkg))' > dune
  $ echo '{0 pkg Page}' > index.mld

  $ cat > foo/dune <<EOF
  > (library
  >  (name foo)
  >  (public_name my_pkg.foo))
  > EOF
  $ echo 'let x = 1' > foo/a.ml
  $ echo 'val x : B.t' > foo/a.mli
  $ echo 'type t = int' > foo/b.ml
  $ echo 'type t = int' > foo/b.mli

  $ cat > bar/dune <<EOF
  > (library
  >  (name bar)
  >  (public_name my_pkg.bar)
  >  (libraries my_pkg.foo))
  > EOF
  $ echo 'let y = Foo.A.x' > bar/bar.ml
  $ echo 'val y : Foo.B.t' > bar/bar.mli

  $ echo '(library (name priv) (package my_pkg))' > priv/dune
  $ echo 'let p = 1' > priv/priv.ml
  $ echo '(library (name solo))' > solo/dune
  $ echo 'let s = 1' > solo/solo.ml

  $ dune build @doc @doc-private

  $ find _build/default -name '*.odoc' | LC_ALL=C sort
  _build/default/_doc/_odoc/pkg/my_pkg/page-index.odoc
  _build/default/bar/.bar.objs/byte/bar.odoc
  _build/default/foo/.foo.objs/byte/foo.odoc
  _build/default/foo/.foo.objs/byte/foo__A.odoc
  _build/default/foo/.foo.objs/byte/foo__B.odoc
  _build/default/priv/.priv.objs/byte/priv.odoc
  _build/default/solo/.solo.objs/byte/solo.odoc

  $ find _build/default/_doc/_html -name '*.html' | LC_ALL=C sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/my_pkg/Bar/index.html
  _build/default/_doc/_html/my_pkg/Foo/A/index.html
  _build/default/_doc/_html/my_pkg/Foo/B/index.html
  _build/default/_doc/_html/my_pkg/Foo/index.html
  _build/default/_doc/_html/my_pkg/Priv/index.html
  _build/default/_doc/_html/my_pkg/index.html
  _build/default/_doc/_html/solo@e3b9de2fb703/Solo/index.html

  $ find _build/default -name '*.odocl' | LC_ALL=C sort
  _build/default/_doc/_odocls/my_pkg/bar.odocl
  _build/default/_doc/_odocls/my_pkg/foo.odocl
  _build/default/_doc/_odocls/my_pkg/page-index.odocl
  _build/default/_doc/_odocls/my_pkg/priv.odocl
  _build/default/_doc/_odocls/solo@e3b9de2fb703/solo.odocl

A package page and a library can both be documented when the library's
public name matches the page's compiled filename, page-example.odoc.

  $ mkdir collision
  $ cd collision
  $ touch dune-workspace
  $ make_dune_project_with_package 3.25 page-example
  $ cat > dune <<EOF
  > (documentation (package page-example))
  > (library (name foo) (public_name page-example.odoc))
  > EOF
  $ echo '{0 Example}' > example.mld
  $ echo 'let x = 1' > foo.ml

  $ dune build @doc
  $ test -f _build/default/_doc/_html/page-example/example.html
  $ test -f _build/default/_doc/_html/page-example/Foo/index.html
