This test generates documentation using odoc for a library:

  $ dune build @doc-new

We index the compiler directory (+ocaml), in which num is optionally present.
To make the test not depend on whether it is installed, we filter the output.

  $ scrub_num () {
  >   grep -v \
  >     -e arith_status.odocl \
  >     -e big_int.odocl \
  >     -e nat.odocl \
  >     -e num.odocl \
  >     -e ratio.odocl
  > }

This test if `.odocl` files are generated
  $ find . -name '*.odocl' | sort -n | scrub_num
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
  ./_build/default/_doc_new/odoc/stdlib/camlinternalFormat.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalFormatBasics.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalLazy.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalMod.odocl
  ./_build/default/_doc_new/odoc/stdlib/camlinternalOO.odocl
  ./_build/default/_doc_new/odoc/stdlib/std_exit.odocl
  ./_build/default/_doc_new/odoc/stdlib/stdlib.odocl

This test if the sherlodoc js files are generated
  $ find . -name '*.js' | sort -n
  ./_build/default/_doc_new/html/docs/db.js
  ./_build/default/_doc_new/html/docs/odoc.support/highlight.pack.js
  ./_build/default/_doc_new/html/docs/odoc.support/katex.min.js
  ./_build/default/_doc_new/html/docs/odoc.support/odoc_search.js
  ./_build/default/_doc_new/html/docs/sherlodoc.js

  $ cat ./_build/default/_doc_new/html/docs/db.js | scrub_num
  /* Sherlodoc DB for: */
  /*   - ../../odoc/stdlib/camlinternalFormat.odocl */
  /*   - ../../odoc/stdlib/camlinternalFormatBasics.odocl */
  /*   - ../../odoc/stdlib/camlinternalLazy.odocl */
  /*   - ../../odoc/stdlib/camlinternalMod.odocl */
  /*   - ../../odoc/stdlib/camlinternalOO.odocl */
  /*   - ../../odoc/stdlib/std_exit.odocl */
  /*   - ../../odoc/stdlib/stdlib.odocl */
  /*   - --favored ../../index/page-docs.odocl */
  /*   - --favored ../../index/local/page-local.odocl */
  /*   - --favored ../../odoc/local/bar/bar.odocl */
  /*   - --favored ../../index/local/bar/page-bar.odocl */
  /*   - --favored ../../odoc/local/foo/foo2.odocl */
  /*   - --favored ../../odoc/local/foo/foo.odocl */
  /*   - --favored ../../index/local/foo/page-foo.odocl */
  /*   - --favored ../../odoc/local/foo/byte/foo_byte.odocl */
  /*   - --favored ../../index/local/foo/byte/page-byte.odocl */
Verify HTML index was generated:

  $ test -f _build/default/_doc_new/html/docs/index.html && echo "index.html exists"
  index.html exists

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!/foo.byte/module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!/bar/module-Bar}.
