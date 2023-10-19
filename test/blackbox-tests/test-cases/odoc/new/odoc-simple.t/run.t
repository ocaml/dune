This test generates documentation using odoc for a library:

  $ dune build @doc-new

This test if `.odocl` files are generated
  $ find _build/default/_doc_new/odoc -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/local/bar/bar.odocl
  _build/default/_doc_new/odoc/local/foo/byte/foo_byte.odocl
  _build/default/_doc_new/odoc/local/foo/foo.odocl
  _build/default/_doc_new/odoc/local/foo/foo2.odocl
  _build/default/_doc_new/odoc/stdlib/camlinternalFormat.odocl
  _build/default/_doc_new/odoc/stdlib/camlinternalFormatBasics.odocl
  _build/default/_doc_new/odoc/stdlib/camlinternalLazy.odocl
  _build/default/_doc_new/odoc/stdlib/camlinternalMod.odocl
  _build/default/_doc_new/odoc/stdlib/camlinternalOO.odocl
  _build/default/_doc_new/odoc/stdlib/std_exit.odocl
  _build/default/_doc_new/odoc/stdlib/stdlib.odocl

  $ dune runtest
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>docs (docs)</title><link rel="stylesheet" href="odoc.support/odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc 2.2.1"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="odoc.support/highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script></head><body class="odoc"><header class="odoc-preamble"><h1 id="docs"><a href="#docs" class="anchor"></a>Docs</h1></header><nav class="odoc-toc"><ul><li><a href="#local-packages">Local Packages</a></li><li><a href="#switch-installed-packages">Switch-installed packages</a></li></ul></nav><div class="odoc-content"><h2 id="local-packages"><a href="#local-packages" class="anchor"></a>Local Packages</h2><ul><li><a href="local/bar/index.html"><code>bar</code></a></li><li><a href="local/foo/index.html"><code>foo</code></a></li></ul><h2 id="switch-installed-packages"><a href="#switch-installed-packages" class="anchor"></a>Switch-installed packages</h2><ul><li><a href="stdlib/index.html"><code>stdlib</code></a></li></ul></div></body></html>

  $ dune build @foo-mld
  {0 Package foo}
  {1 Sub-indexes}
  - {{!page-"byte"}byte}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:foo foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 Package bar}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
