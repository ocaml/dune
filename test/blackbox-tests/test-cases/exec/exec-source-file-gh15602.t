Regression test for GH-15602: `dune exec` used to hang when asked to execute a
source file.

  $ make_dune_project 3.24

  $ mkdir bin
  $ cat >bin/dune <<'EOF'
  > (executable
  >  (name main))
  > EOF

  $ cat >bin/main.ml <<'EOF'
  > let () = print_endline "Hello, World!"
  > EOF

The executable runs normally:

  $ dune exec -- ./bin/main.exe
  Hello, World!

Trying to execute the source file exits with an error. The timeout guards
against the command hanging:

  $ $timeout --signal=KILL 5 dune exec ./bin/main.ml
  Error: execve(./_build/default/bin/main.ml): Permission denied
  [1]
