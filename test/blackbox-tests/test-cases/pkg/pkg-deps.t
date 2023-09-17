We should be able to specify (package ..) deps on locally built packages.

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/foo.pkg <<EOF
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (system "command -v foo"))
  >  (deps (package foo)))
  > EOF

  $ dune build @foo
  File "dune", line 4, characters 16-19:
  4 |  (deps (package foo)))
                      ^^^
  Error: Package foo does not exist
  [1]
