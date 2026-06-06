Copies `runtime_deps` that already live under the Melange target directory.


  $ make_melange_project 3.8 0.1
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
