Test that depending on a non-existent package produces an error with a
proper source location.

  $ make_dune_project 3.24

  $ cat >dune <<EOF
  > (rule
  >  (deps (package nonexistent))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

  $ dune build out 2>&1
  File "dune", line 2, characters 16-27:
  2 |  (deps (package nonexistent))
                      ^^^^^^^^^^^
  Error: Package nonexistent does not exist
  [1]
