Checks `with-accepted-exit-codes` against matching and failing commands.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias a)
  >  (action (with-accepted-exit-codes 0 (run dune_cmd exit-code 1))))
  > EOF

  $ dune build --display=short --root . @a
  File "dune", lines 1-3, characters 0-83:
  1 | (rule
  2 |  (alias a)
  3 |  (action (with-accepted-exit-codes 0 (run dune_cmd exit-code 1))))
      dune_cmd (anonymous) (exit 1)
  [1]

  $ cat >> dune <<EOF
  > (rule
  >  (alias b)
  >  (action (with-accepted-exit-codes (not 0) (run dune_cmd exit-code 1))))
  > EOF

  $ dune build --display=short --root . @b
      dune_cmd (anonymous)

  $ cat >> dune <<EOF
  > (rule
  >  (alias c)
  >  (action (with-accepted-exit-codes (or 1 2 3) (run dune_cmd exit-code 2))))
  > (rule
  >  (alias d)
  >  (action (with-accepted-exit-codes (or 4 5 6) (run dune_cmd exit-code 7))))
  > EOF

  $ dune build --display=short --root . @c
      dune_cmd (anonymous)

  $ dune build --display=short --root . @d
  File "dune", lines 10-12, characters 0-92:
  10 | (rule
  11 |  (alias d)
  12 |  (action (with-accepted-exit-codes (or 4 5 6) (run dune_cmd exit-code 7))))
      dune_cmd (anonymous) (exit 7)
  [1]

  $ cat >> dune <<EOF
  > (rule
  >  (alias e)
  >  (action (with-accepted-exit-codes (not 0) (dynamic-run dune_cmd exit-code 1))))
  > EOF

The accepted code does not excuse failing to initialize the DAP session.

  $ dune build --root . @e > e.output 2>&1; echo $?
  1
  $ tr '\n' ' ' < e.output | grep -qF 'failed to respond to dune.'
