Dune's actions may produce trace events

  $ cat >dune-project<<EOF
  > (lang dune 3.22)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run action_trace -name foo -cat bar -arg baz)))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq -s 'include "dune"; redactedActionTraces'
  {
    "cat": "bar",
    "name": "foo",
    "ts": 0,
    "args": {
      "arg": "baz",
      "digest": "REDACTED"
    }
  }
