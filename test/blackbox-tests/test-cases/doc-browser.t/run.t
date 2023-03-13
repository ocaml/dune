This tests shows how to use the `dune ocaml doc` command to open the
documentation index to a browser.
  $ ls
  dune
  dune-project
  foo.ml
  spoof.desktop
  xdg-open
  $ PATH=.:$PATH 
  $ export PATH
  $ dune ocaml doc | grep -c "file:///.*/_build/.sandbox/.*/default/test/blackbox-tests/test-cases/doc-browser.t/_build/default/_doc/_html/index.html"
  1
