The plugin's exit mode terminates after DAP initialization. By default, zero
is accepted and a nonzero exit code fails the action.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ cat > dune <<'EOF'
  > (rule
  >  (alias zero)
  >  (action (dynamic-run ./foo.exe exit 0)))
  > (rule
  >  (alias nonzero)
  >  (action (dynamic-run ./foo.exe exit 7)))
  > EOF
  $ dune build @zero
  $ dune build @nonzero
  File "dune", lines 4-6, characters 0-64:
  4 | (rule
  5 |  (alias nonzero)
  6 |  (action (dynamic-run ./foo.exe exit 7)))
  Command exited with code 7.
  [1]

An explicitly accepted code succeeds after DAP initialization.

  $ cat > dune <<'EOF'
  > (rule
  >  (alias accepted)
  >  (action (with-accepted-exit-codes 7 (dynamic-run ./foo.exe exit 7))))
  > EOF
  $ dune build @accepted

Zero fails when only seven is accepted.

  $ cat > dune <<'EOF'
  > (rule
  >  (alias rejected)
  >  (action (with-accepted-exit-codes 7 (dynamic-run ./foo.exe exit 0))))
  > EOF
  $ dune build @rejected
  File "dune", lines 1-3, characters 0-94:
  1 | (rule
  2 |  (alias rejected)
  3 |  (action (with-accepted-exit-codes 7 (dynamic-run ./foo.exe exit 0))))
  Command exited with code 0.
  [1]
