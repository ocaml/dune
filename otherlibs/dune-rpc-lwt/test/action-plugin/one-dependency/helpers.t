Helpers inherit the parent's action ID and open independent RPC connections.
Their dependencies share the action's facts and scheduler slot, including when
a later helper reconnects with the same ID.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ printf first > first
  $ printf second > second
  $ cat > dune <<'EOF'
  > (rule
  >  (target helper-first)
  >  (deps first)
  >  (action (run sh -c "cat first > helper-first")))
  > (rule
  >  (target helper-second)
  >  (deps second)
  >  (action (run sh -c "cat second > helper-second")))
  > (rule
  >  (target output)
  >  (action (with-stdout-to output (dynamic-run ./foo.exe helpers))))
  > EOF
  $ timeout 5 dune build -j 1 output
  $ cat _build/default/output
  first
  second
  first
  $ printf changed > first
  $ timeout 5 dune build -j 1 output
  $ cat _build/default/output
  changed
  second
  changed
  $ printf updated > second
  $ timeout 5 dune build -j 1 output
  $ cat _build/default/output
  changed
  updated
  changed

An action ID expires when its action finishes, even while the Dune instance
continues running. A later action cannot reconnect using the expired ID.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target action-id)
  >  (action (with-stdout-to action-id (dynamic-run ./foo.exe action-id))))
  > (rule
  >  (alias expired)
  >  (deps action-id)
  >  (action (dynamic-run ./foo.exe expired)))
  > EOF
  $ dune build @expired
  expired action rejected

A helper catching a required-dependency failure cannot hide it from the action.

  $ cat >> dune <<'EOF'
  > (rule
  >  (alias helper-failure)
  >  (action (dynamic-run ./foo.exe helper-failure)))
  > EOF
  $ dune build @helper-failure > helper.output 2>&1
  [1]
  $ grep -q 'No rule found for missing-helper-input' helper.output
