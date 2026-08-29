Invalid traces should be an error

  $ make_dune_project 3.22

  $ cat >dune <<'EOF'
  > (rule
  >  (alias foo)
  >  (action (bash "mkdir $DUNE_ACTION_TRACE_DIR && echo foo > $DUNE_ACTION_TRACE_DIR/trace.csexp")))
  > EOF

  $ dune build @foo 2>&1 | dune_cmd subst '_build/.*' '_build/REDACTED'
  File "dune", lines 1-3, characters 0-116:
  1 | (rule
  2 |  (alias foo)
  3 |  (action (bash "mkdir $DUNE_ACTION_TRACE_DIR && echo foo > $DUNE_ACTION_TRACE_DIR/trace.csexp")))
  Error: invalid action trace in
  _build/REDACTED
  [1]

A failed action currently leaves its trace event uncollected.

  $ cat >dune <<'EOF'
  > (rule
  >  (alias failed)
  >  (action
  >   (bash "action_trace -name failed -cat bar -arg baz; exit 1")))
  > EOF

  $ dune build @failed >/dev/null 2>&1 ||
  >   dune trace cat --only-actions |
  >   jq_dune -s '
  >     redactedActionTraces | select(.name == "failed")
  >   '
