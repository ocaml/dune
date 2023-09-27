This tests shows how to use the `dune ocaml doc` command to open the
documentation index to a browser.
  $ export PATH=.:$PATH 
  $ dune ocaml doc | sed -e 's|.*file://\([^ ]*\).*|\1|'
  Docs built. Index can be found here:
  $TESTCASE_ROOT/_build/default/_doc/_html/index.html
  
  $TESTCASE_ROOT/_build/default/_doc/_html/index.html
