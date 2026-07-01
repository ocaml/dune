%{bin:...} in cram (deps ...) makes the binary callable by name.

  $ make_mypkg_bin_project
  $ mkdir tests

  $ cat >tests/dune <<'EOF'
  > (cram (deps %{bin:mybin}))
  > EOF
  $ cat >tests/run.t <<'EOF'
  >   $ mybin
  > EOF

  $ dune runtest tests
  File "tests/run.t", line 1, characters 0-0:
  --- tests/run.t
  +++ tests/run.t.corrected
  @@ -1 +1,2 @@
     $ mybin
  +  hello from mybin
  [1]
