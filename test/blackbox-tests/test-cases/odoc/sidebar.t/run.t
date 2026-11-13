A global sidebar lists the pages and libraries of a package. A private library
without a package has no sidebar.

  $ dune build @doc @doc-private

Pages and modules of the package have a sidebar listing them:

  $ grep -o '<nav class="odoc-toc odoc-global-toc">.*</nav>' \
  >   _build/default/_doc/_html/my_pkg/index.html \
  >   _build/default/_doc/_html/my_pkg/my_pkg.foo/Foo/index.html
  _build/default/_doc/_html/my_pkg/index.html:<nav class="odoc-toc odoc-global-toc"><ul><li><a href="#" class="current_unit">my_pkg</a><ul><li><a href="guide.html">Guide</a></li><li>my_pkg.__private__.priv<ul><li><a href="my_pkg.__private__.priv/Priv/index.html">Priv</a></li></ul></li><li>my_pkg.bar<ul><li><a href="my_pkg.bar/Bar/index.html">Bar</a></li></ul></li><li>my_pkg.foo<ul><li><a href="my_pkg.foo/Foo/index.html">Foo</a></li></ul></li></ul></li></ul></nav>
  _build/default/_doc/_html/my_pkg/my_pkg.foo/Foo/index.html:<nav class="odoc-toc odoc-global-toc"><ul><li><a href="../../index.html">my_pkg</a><ul><li><a href="../../guide.html">Guide</a></li><li>my_pkg.__private__.priv<ul><li><a href="../../my_pkg.__private__.priv/Priv/index.html">Priv</a></li></ul></li><li>my_pkg.bar<ul><li><a href="../../my_pkg.bar/Bar/index.html">Bar</a></li></ul></li><li>my_pkg.foo<ul><li><a href="#" class="current_unit">Foo</a></li></ul></li></ul></li></ul></nav>

Nor does a private library without a package:

  $ grep -o '<nav class="odoc-toc odoc-global-toc">.*</nav>' \
  >   _build/default/_doc/_html/solo@*/Solo/index.html
  [1]
