`dune fmt` should reformat an unformatted file every time it is invoked. In
particular, if a file is reverted to an unformatted state after a previous
`dune fmt` run, the next `dune fmt` must detect this and reformat the file
again. The test below exercises that scenario.

  $ make_dune_project 3.25

Start with an unformatted dune file.

  $ cat > dune << EOF
  > (rule (write-file a b))
  > EOF

The first invocation of `dune fmt` should report the diff and promote the
formatted version.

  $ dune fmt
  File "dune", line 1, characters 0-0:
  --- dune
  +++ dune.corrected
  @@ -1 +1,2 @@
  -(rule (write-file a b))
  +(rule
  + (write-file a b))
  Promoting _build/default/dune.corrected to dune.
  [1]

  $ cat dune
  (rule
   (write-file a b))

Now rewrite the file back to its original unformatted state and run `dune fmt`
again. We expect the same behaviour as the first invocation: the diff is
reported and the file is reformatted.

  $ cat > dune << EOF
  > (rule (write-file a b))
  > EOF

CR-soon Alizter: dune fmt is not re-running and this is a bug.

  $ dune fmt
  File "dune", line 1, characters 0-0:
  --- dune
  +++ dune.corrected
  @@ -1 +1,2 @@
  -(rule (write-file a b))
  +(rule
  + (write-file a b))
  Promoting _build/default/dune.corrected to dune.
  [1]
  $ cat dune
  (rule
   (write-file a b))
