Dune's actions may produce trace events

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run action_trace -name foo -cat bar -arg baz)))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq_dune -s 'redactedActionTraces'
  {
    "cat": "bar",
    "name": "foo",
    "ts": 0,
    "args": {
      "arg": "baz",
      "digest": "REDACTED"
    }
  }
