A global sidebar lists the pages and libraries of a package. A private library
without a package has no sidebar.

  $ dune build @doc @doc-private

Pages and modules of the package have no sidebar yet:

  $ grep -o '<nav class="odoc-toc odoc-global-toc">.*</nav>' \
  >   _build/default/_doc/_html/my_pkg/index.html \
  >   _build/default/_doc/_html/my_pkg/my_pkg.foo/Foo/index.html
  [1]

Nor does a private library without a package:

  $ grep -o '<nav class="odoc-toc odoc-global-toc">.*</nav>' \
  >   _build/default/_doc/_html/solo@*/Solo/index.html
  [1]
