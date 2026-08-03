Rejects nested action modifiers under `with-accepted-exit-codes` before Dune 2.2.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias a)
  >  (action (with-accepted-exit-codes
  >           1
  >           (with-stdout-to out.txt
  >            (run dune_cmd exit-code 1)))))
  > EOF

  $ dune build --root . @a
  File "dune", lines 5-6, characters 10-72:
  5 |           (with-stdout-to out.txt
  6 |            (run dune_cmd exit-code 1)))))
  Error: nesting modifiers under 'with-accepted-exit-codes' is only available
  since version 2.2 of the dune language. Please update your dune-project file
  to have (lang dune 2.2).
  [1]
