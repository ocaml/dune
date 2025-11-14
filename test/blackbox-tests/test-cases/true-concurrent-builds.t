Test that builds work correctly with the new Build_coordinator architecture.

Setup project with multiple independent targets:

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target a.txt)
  >  (action (bash "echo 'Building A'; sleep 0.05; echo A > a.txt")))
  > (rule
  >  (target b.txt)
  >  (action (bash "echo 'Building B'; sleep 0.05; echo B > b.txt")))
  > (rule
  >  (target c.txt)
  >  (action (bash "echo 'Building C'; sleep 0.05; echo C > c.txt")))
  > (alias
  >  (name build-a)
  >  (deps a.txt))
  > (alias
  >  (name build-b)
  >  (deps b.txt))
  > (alias
  >  (name build-c)
  >  (deps c.txt))
  > EOF

Test 1: Basic builds work with new architecture

  $ dune build @build-a
  Building A

  $ dune build @build-b
  Building B

  $ cat _build/default/a.txt
  A
  $ cat _build/default/b.txt
  B

Test 2: Sequential builds complete correctly

  $ dune clean
  $ dune build @build-a @build-b @build-c
  Building A
  Building B
  Building C

  $ ls _build/default/*.txt
  _build/default/a.txt
  _build/default/b.txt
  _build/default/c.txt

With the new Build_coordinator architecture, each build gets its own
isolated session with separate progress, errors, and hooks. This enables
RPC and watch mode builds to execute concurrently without serializing
at a mutex.

Note: Cram tests are sequential by nature and can't demonstrate true
parallelism. Full concurrent execution testing requires RPC + watch mode
in a real environment.
