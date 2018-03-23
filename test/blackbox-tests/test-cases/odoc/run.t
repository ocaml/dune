  $ $JBUILDER build @doc -j1 --display short --root .
      ocamldep bar.ml.d
        ocamlc .bar.objs/bar.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/bar/bar.odoc
          odoc _doc/_html/bar/Bar/.jbuilder-keep,_doc/_html/bar/Bar/index.html
          odoc _doc/_odoc/pkg/bar/page-index.odoc
          odoc _doc/_html/bar/index.html
          odoc _doc/_html/odoc.css
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo/foo.odoc
      ocamldep foo_byte.ml.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo.byte/foo_byte.odoc
      ocamldep foo2.ml.d
        ocamlc .foo.objs/foo2.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo/foo2.odoc
          odoc _doc/_html/foo/Foo/.jbuilder-keep,_doc/_html/foo/Foo/index.html
          odoc _doc/_odoc/pkg/foo/page-index.odoc
          odoc _doc/_html/foo/index.html
          odoc _doc/_html/foo/Foo_byte/.jbuilder-keep,_doc/_html/foo/Foo_byte/index.html
          odoc _doc/_html/foo/Foo2/.jbuilder-keep,_doc/_html/foo/Foo2/index.html
  $ $JBUILDER runtest -j1 --display short --root .
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>index</title>
      <link rel="stylesheet" href="./odoc.css"/>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    </head>
    <body>
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      <li><a href="bar/index.html">bar</a></li>
      <li><a href="foo/index.html">foo</a></li>
      </ol>
      </div>
    </body>
  </html>

  $ $JBUILDER build @foo-mld -j1 --display short --root .
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}.
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ $JBUILDER build @bar-mld -j1 --display short --root .
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
