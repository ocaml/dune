This test generates documentation using odoc for a library:

  $ dune build @doc-new

This test if `.odocl` files are generated
  $ find _build/default/_doc_new/odoc -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/bar/bar.odocl
  _build/default/_doc_new/odoc/foo/byte/foo_byte.odocl
  _build/default/_doc_new/odoc/foo/foo.odocl
  _build/default/_doc_new/odoc/foo/foo2.odocl
  _build/default/_doc_new/odoc/ocaml/arith_status.odocl
  _build/default/_doc_new/odoc/ocaml/big_int.odocl
  _build/default/_doc_new/odoc/ocaml/bigarray.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalAtomic.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalFormat.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalFormatBasics.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalLazy.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalMod.odocl
  _build/default/_doc_new/odoc/ocaml/camlinternalOO.odocl
  _build/default/_doc_new/odoc/ocaml/dynlink.odocl
  _build/default/_doc_new/odoc/ocaml/nat.odocl
  _build/default/_doc_new/odoc/ocaml/num.odocl
  _build/default/_doc_new/odoc/ocaml/profiling.odocl
  _build/default/_doc_new/odoc/ocaml/ratio.odocl
  _build/default/_doc_new/odoc/ocaml/std_exit.odocl
  _build/default/_doc_new/odoc/ocaml/stdlib.odocl
  _build/default/_doc_new/odoc/ocaml/str.odocl
  _build/default/_doc_new/odoc/ocaml/topdirs.odocl
  _build/default/_doc_new/odoc/ocaml/unix.odocl
  _build/default/_doc_new/odoc/ocaml/unixLabels.odocl

  $ dune runtest
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>docs (docs)</title><link rel="stylesheet" href="odoc.support/odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc 2.2.1"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="odoc.support/highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script></head><body class="odoc"><header class="odoc-preamble"><h1 id="docs"><a href="#docs" class="anchor"></a>Docs</h1></header><nav class="odoc-toc"><ul><li><a href="#local-packages">Local Packages</a></li><li><a href="#other-switch-library-directories">Other switch library directories</a></li></ul></nav><div class="odoc-content"><h2 id="local-packages"><a href="#local-packages" class="anchor"></a>Local Packages</h2><ul><li><a href="bar/index.html"><code>bar</code></a></li><li><a href="foo/index.html"><code>foo</code></a></li></ul><h2 id="other-switch-library-directories"><a href="#other-switch-library-directories" class="anchor"></a>Other switch library directories</h2><ul><li><a href="ocaml/index.html"><code>ocaml</code></a></li></ul></div></body></html>

  $ dune build @foo-mld
  {0 index index}
  {1 Sub-packages}
  - {{!page-"byte"}foo.byte}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 index index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
