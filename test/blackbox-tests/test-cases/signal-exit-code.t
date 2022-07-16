Demonstrate the exit code once dune is interrupted by a signal

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias all)
  >  (action (system "kill -s INT \$PPID")))
  > EOF

  $ dune build @all
  [130]
