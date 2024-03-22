Show that `melange.emit` + correct dependency tracking reads the processed
file after any dialects have run

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (using melange 0.1)
  > (dialect
  >  (name myd)
  >  (implementation
  >   (preprocess (run cat %{input-file}))
  >   (extension myd)))
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
  $ cat > lib/foo.myd <<EOF
  > let name = Bar.name
  > EOF
  $ cat > lib/bar.ml <<EOF
  > let name = "Zoe"
  > EOF
  $ dune build @mel

Now try preprocessing too

  $ dune clean
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (preprocess (action (run cat %{input-file})))
  >  (modes melange))
  > EOF
  $ dune build @mel
