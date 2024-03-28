Show missing dependency in Melange when sandboxing is enabled

  $ export DUNE_SANDBOX=symlink
  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (libraries foo)
  >  (emit_stdlib false))
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let name = Bar.name
  > EOF

  $ cat > lib/bar.ml <<EOF
  > let name = "Zoe"
  > EOF

  $ dune build @mel

