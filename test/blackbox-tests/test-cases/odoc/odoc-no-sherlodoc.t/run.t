This test generates documentation using odoc for a library, and hides any
sherlodoc installation to test what happens when sherlodoc is not installed.

Set up some fake environment without sherlodoc

  $ mkdir _path
  $ ln -s $(command -v dune) _path/
  $ ln -s $(command -v ocamlc) _path/
  $ ln -s $(command -v ocamldep) _path/
  $ ln -s $(command -v odoc) _path/
  $ PATH=$(realpath ./_path) dune build @doc

This test if `.odocl` files are generated
  $ find _build/default/_doc/_odocls -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/bar/bar.odocl
  _build/default/_doc/_odocls/bar/page-index.odocl
  _build/default/_doc/_odocls/foo/foo.odocl
  _build/default/_doc/_odocls/foo/foo2.odocl
  _build/default/_doc/_odocls/foo/foo_byte.odocl
  _build/default/_doc/_odocls/foo/page-index.odocl

  $ find . -name '*.js' | sort -n
  ./_build/default/_doc/_html/odoc.support/highlight.pack.js
  ./_build/default/_doc/_html/odoc.support/katex.min.js
  ./_build/default/_doc/_html/odoc.support/odoc_search.js


  $ PATH=$(realpath ./_path) dune runtest
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>index</title>
      <link rel="stylesheet" href="./odoc.support/odoc.css"/>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    </head>
    <body>
      <main class="content">
        <div class="by-name">
        <h2>OCaml package documentation</h2>
        <ol>
        <li><a href="bar/index.html">bar</a></li>
        <li><a href="foo/index.html">foo</a></li>
        </ol>
        </div>
      </main>
    </body>
  </html>

  $ PATH=$(realpath ./_path) dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ PATH=$(realpath ./_path) dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.

