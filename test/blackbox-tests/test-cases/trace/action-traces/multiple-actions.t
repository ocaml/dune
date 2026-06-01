Dune should collect traces from all actions

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run action_trace -name foo2 -cat bar -arg baz)))
  > (rule
  >  (alias foo)
  >  (action (run action_trace -name foo1 -cat bar -arg baz -dur 20)))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq -s 'include "dune"; redactedActionTraces'
  {
    "cat": "bar",
    "name": "foo1",
    "ts": 0,
    "args": {
      "dur": 20,
      "arg": "baz",
      "digest": "REDACTED"
    }
  }
  {
    "cat": "bar",
    "name": "foo2",
    "ts": 0,
    "args": {
      "arg": "baz",
      "digest": "REDACTED"
    }
  }
