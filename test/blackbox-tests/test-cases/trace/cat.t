dune trace cat can be used to view the trace:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ dune build
  $ dune trace cat | jq -c 'keys'
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","dur","name","ts"]
  ["args","cat","name","ts"]

Test the --sexp flag outputs S-expressions (not JSON):

  $ dune trace cat --sexp | head -1 | grep -q "^(" && echo "S-expression output detected"
  S-expression output detected

Test the --chrome-trace flag outputs Chrome event format as a JSON array with
some typical fields:

  $ dune trace cat --chrome-trace | jq 'type'
  "array"

Test that --sexp and --chrome-trace are mutually exclusive:

  $ dune trace cat --sexp --chrome-trace 2>&1 | head -1
  Error: --chrome-trace and --sexp are mutually exclusive


All the event types from chrome and field per type:

  $ dune trace cat --chrome | jq 'group_by(.ph) | map({value: .[0].ph, representative: .[0] | keys})'
  [
    {
      "value": "X",
      "representative": [
        "args",
        "cat",
        "dur",
        "name",
        "ph",
        "pid",
        "ts"
      ]
    },
    {
      "value": "i",
      "representative": [
        "args",
        "cat",
        "name",
        "ph",
        "pid",
        "ts"
      ]
    }
  ]
