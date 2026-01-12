Dune actions may produce multiple trace events

  $ cat >dune-project<<EOF
  > (lang dune 3.22)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action
  >   (progn
  >    (run action_trace -name foo1 -cat bar -arg baz -dur 20)
  >    (run action_trace -name foo2 -cat bar -arg baz))))
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
