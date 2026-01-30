Demonstrate the exit code once dune is interrupted by a signal

  $ make_dune_project 3.2

  $ cat >dune <<EOF
  > (rule
  >  (alias all)
  >  (action (system "kill -s INT \$PPID")))
  > EOF

  $ dune build @all
  [130]
