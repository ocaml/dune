Installing a library with `-x foo` should install the library for that context

  $ mkdir prefix
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf


  $ dune build @install -x foo

  $ dune install --dry-run --prefix prefix --display short -p repro -x foo
  Error: The following <package>.install are missing:
  - _build/default/repro.install
  - _build/default.foo/repro.install
  Hint: try running: dune build [-p <pkg>] @install
  [1]
