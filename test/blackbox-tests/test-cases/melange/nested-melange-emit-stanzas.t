Make sure an error is returned if trying to nest `melange.emit` stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF
  $ mkdir -p a/b/c
  $ cat > a/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_system commonjs))
  > EOF
  $ cat > a/b/c/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_system commonjs))
  > EOF

  $ dune build @mel
