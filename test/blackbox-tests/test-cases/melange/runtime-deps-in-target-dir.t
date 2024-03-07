
  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  $ mkdir output

  $ cat > output/foo.txt <<EOF
  > hello foo
  > EOF
  $ cat > dune << EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (runtime_deps output/foo.txt))
  > EOF

  $ dune build @mel

  $ ls _build/default/output
  foo.txt
  output
  $ ls _build/default/output/output
  foo.txt
