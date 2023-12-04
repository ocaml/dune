This tests shows how to use the `dune ocaml doc` command to open the
documentation index to a browser.
  $ if [ "$(uname)" = Darwin ]; then mv xdg-open open; fi
  $ export PATH=.:$PATH 
  $ dune ocaml doc | sed -e 's|^-u *||'
  Docs built. Index can be found here: _build/default/_doc/_html/index.html
  open command received args:
  file://_build/default/_doc/_html/index.html
