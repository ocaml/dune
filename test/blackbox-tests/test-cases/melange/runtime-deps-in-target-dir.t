
  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF
  $ mkdir output

  $ cat > output/foo.txt <<EOF
  > hello foo
  > EOF
  $ cat > dune << EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (runtime_deps output/foo.txt))
  > EOF

  $ dune build @melange

  $ ls _build/default/output
  foo.txt
  output
  $ ls _build/default/output/output
  foo.txt
