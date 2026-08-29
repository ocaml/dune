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

Action events may be selected explicitly.

  $ dune trace cat --only-actions | jq_dune -s 'redactedActionTraces'
  {
    "cat": "bar",
    "name": "foo",
    "ts": 0,
    "args": {
      "arg": "baz",
      "digest": "REDACTED"
    }
  }

Chrome output applies the same selection.

  $ dune trace cat --only-actions --chrome-trace | jq '
  >   .[] | .ts = 0 | .pid = 0 | .args.digest = "REDACTED"
  > '
  {
    "cat": "bar",
    "name": "foo",
    "ts": 0,
    "args": {
      "arg": "baz",
      "digest": "REDACTED"
    },
    "ph": "i",
    "pid": 0
  }

Excluding action events retains Dune's own trace events.

  $ dune trace cat --no-actions | jq -s '
  >   first
  > | .ts = 0
  > | .args.argv = ["REDACTED"]
  > | .args.env = ["REDACTED"]
  > | .args.root = "REDACTED"
  > | .args.pid = 0
  > | .args.initial_cwd = "REDACTED"
  > | .args.start = 0
  > '
  {
    "cat": "config",
    "name": "init",
    "ts": 0,
    "args": {
      "build_dir": "_build",
      "argv": [
        "REDACTED"
      ],
      "env": [
        "REDACTED"
      ],
      "root": "REDACTED",
      "pid": 0,
      "initial_cwd": "REDACTED",
      "start": 0
    }
  }

There are no action events in the filtered output.

  $ dune trace cat --no-actions | jq_dune -s 'redactedActionTraces'

The selection flags are mutually exclusive.

  $ dune trace cat --no-actions --only-actions
  Usage: dune trace cat [--help] [OPTION]…
  dune: options '--no-actions' and '--only-actions' cannot be present at the
        same time
  [1]
