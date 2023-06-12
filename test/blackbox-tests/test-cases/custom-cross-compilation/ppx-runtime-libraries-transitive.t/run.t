Demonstrate a bad interaction between cross-compilation and
ppx_runtime_libraries

  $ mkdir -p etc/findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf
  $ touch etc/findlib.conf etc/findlib.conf.d/foo.conf

  $ mkdir lib
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name ppx-cross))
  > EOF

  $ cat > lib/dune <<EOF
  > (library
  >  (name lib)
  >  (public_name ppx-cross.lib)
  >  (preprocess (pps other-ppx.eq)))
  > EOF
  $ touch lib/lib.ml

  $ dune build @install -x foo

