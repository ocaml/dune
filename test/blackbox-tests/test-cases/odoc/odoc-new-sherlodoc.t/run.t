This test generates documentation using odoc for a library:

  $ dune build @doc-new

This test if `.odocl` files are generated
  $ find . -name '*.odocl' | sort -n
  ./_build/default/_doc_new/index/local/bar/page-bar.odocl
  ./_build/default/_doc_new/index/local/foo/byte/page-byte.odocl
  ./_build/default/_doc_new/index/local/foo/page-foo.odocl
  ./_build/default/_doc_new/index/local/page-local.odocl
  ./_build/default/_doc_new/index/page-docs.odocl
  ./_build/default/_doc_new/index/stdlib/page-stdlib.odocl
  ./_build/default/_doc_new/odoc/local/bar/bar.odocl
  ./_build/default/_doc_new/odoc/local/foo/byte/foo_byte.odocl
  ./_build/default/_doc_new/odoc/local/foo/foo.odocl
  ./_build/default/_doc_new/odoc/local/foo/foo2.odocl
  ./_build/default/_doc_new/odoc/stdlib/arith_status.odocl
  ./_build/default/_doc_new/odoc/stdlib/big_int.odocl
  ./_build/default/_doc_new/odoc/stdlib/bigarray.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalAtomic.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalFormat.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalFormatBasics.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalLazy.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalMod.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalOO.odocl
  ./_build/default/_doc_new/odoc/stdlib/dynlink.odocl
  ./_build/default/_doc_new/odoc/stdlib/nat.odocl
  ./_build/default/_doc_new/odoc/stdlib/num.odocl
  ./_build/default/_doc_new/odoc/stdlib/profiling.odocl
  ./_build/default/_doc_new/odoc/stdlib/ratio.odocl
  ./_build/default/_doc_new/odoc/stdlib/std_exit.odocl
  ./_build/default/_doc_new/odoc/stdlib/stdlib.odocl
  ./_build/default/_doc_new/odoc/stdlib/str.odocl
  ./_build/default/_doc_new/odoc/stdlib/topdirs.odocl
  ./_build/default/_doc_new/odoc/stdlib/unix.odocl
  ./_build/default/_doc_new/odoc/stdlib/unixLabels.odocl

This test if the sherlodoc js files are generated
  $ find . -name '*.js' | sort -n
  ./_build/default/_doc_new/html/docs/db.js
  ./_build/default/_doc_new/html/docs/odoc.support/highlight.pack.js
  ./_build/default/_doc_new/html/docs/odoc.support/katex.min.js
  ./_build/default/_doc_new/html/docs/odoc.support/odoc_search.js
  ./_build/default/_doc_new/html/docs/sherlodoc.js

  $ cat ./_build/default/_doc_new/html/docs/db.js
  /* Sherlodoc DB for: */
  /*   - ../../odoc/stdlib/arith_status.odocl */
  /*   - ../../odoc/stdlib/big_int.odocl */
  /*   - ../../odoc/stdlib/bigarray.odocl */
  /*   - ../../odoc/stdlib/camlinternalAtomic.odocl */
  /*   - ../../odoc/stdlib/camlinternalFormat.odocl */
  /*   - ../../odoc/stdlib/camlinternalFormatBasics.odocl */
  /*   - ../../odoc/stdlib/camlinternalLazy.odocl */
  /*   - ../../odoc/stdlib/camlinternalMod.odocl */
  /*   - ../../odoc/stdlib/camlinternalOO.odocl */
  /*   - ../../odoc/stdlib/dynlink.odocl */
  /*   - ../../odoc/stdlib/nat.odocl */
  /*   - ../../odoc/stdlib/num.odocl */
  /*   - ../../odoc/stdlib/profiling.odocl */
  /*   - ../../odoc/stdlib/ratio.odocl */
  /*   - ../../odoc/stdlib/std_exit.odocl */
  /*   - ../../odoc/stdlib/stdlib.odocl */
  /*   - ../../odoc/stdlib/str.odocl */
  /*   - ../../odoc/stdlib/topdirs.odocl */
  /*   - ../../odoc/stdlib/unix.odocl */
  /*   - ../../odoc/stdlib/unixLabels.odocl */
  /*   - --favored ../../index/page-docs.odocl */
  /*   - --favored ../../index/local/page-local.odocl */
  /*   - --favored ../../odoc/local/bar/bar.odocl */
  /*   - --favored ../../index/local/bar/page-bar.odocl */
  /*   - --favored ../../odoc/local/foo/foo2.odocl */
  /*   - --favored ../../odoc/local/foo/foo.odocl */
  /*   - --favored ../../index/local/foo/page-foo.odocl */
  /*   - --favored ../../odoc/local/foo/byte/foo_byte.odocl */
  /*   - --favored ../../index/local/foo/byte/page-byte.odocl */
  $ dune runtest
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

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
