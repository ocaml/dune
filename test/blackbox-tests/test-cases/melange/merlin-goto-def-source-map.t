Show Melange-specific Merlin configuration for source lookup

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 1.0)
  > 
  > (dialect
  >  (name alpha)
  >  (implementation
  >   (extension alpha))
  >  (interface
  >   (extension alphai)))
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries lib)
  >  (modules main))
  > EOF
  $ cat > main.ml <<EOF
  > let _ = Foo.foo
  > let _ = Bar.foo
  > EOF
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let foo = "ocaml"
  > EOF
  $ cat > lib/foo.melange.ml <<EOF
  > let foo = "melange"
  > EOF
  $ cat > lib/bar.alpha <<EOF
  > let foo = "dialect"
  > EOF
  $ cat > lib/bar.melange.alpha <<EOF
  > let foo = "melange dialect"
  > EOF

  $ dune build @all @check
  $ dune ocaml dump-dot-merlin "$PWD" > merlin.conf
  $ cat merlin.conf | grep "S $PWD"
  S $TESTCASE_ROOT
  S $TESTCASE_ROOT/lib
  $ cat merlin.conf | grep '^SUFFIX '
  SUFFIX .melange.alpha .melange.alphai
  SUFFIX .melange.ml .melange.mli
  SUFFIX .melange.re .melange.rei
  SUFFIX .alpha .alphai
