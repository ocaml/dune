Syntax error inside a cram command
  $ mkdir foo && cd foo
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >t1.t <<EOF
  >   $ foo-bar() { true; }
  > EOF

  $ dune runtest --auto-promote
            sh (internal) (exit 2)
  (cd _build/.sandbox/cdb38568b2ab5eaeeab253debbcff1a1/default && /run/current-system/sw/bin/sh /var/folders/nc/x9_nmmsj0rjbfyzxb_kjk6qr0000gn/T/build_75e3fc_dune/dune_cram_b748db_.t1.t/main.sh)
  -> required by alias runtest
  [1]
