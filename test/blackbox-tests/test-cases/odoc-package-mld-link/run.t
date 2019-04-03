Make sure that links between mld files are resolved even when there is
no library associated with the project

This test case is based on code provided by @vphantom, ocaml/dune#2007

  $ dune build @doc

  $ grep -r xref-unresolved _build/default/_doc/_html/odoc_page_link_bug/index.html
  [1]

