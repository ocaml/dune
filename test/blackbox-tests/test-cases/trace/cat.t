dune trace cat can be used to view the trace:

  $ make_dune_project 3.21
  $ dune build
  $ dune trace cat | jq -c -s 'map(keys) | unique[]'
  ["args","cat","dur","name","ts"]
  ["args","cat","name","ts"]

Test the --sexp flag outputs S-expressions (not JSON):

  $ dune trace cat --sexp | head -1 | cut -c1 || true
  (

Test the --chrome-trace flag outputs Chrome event format as a JSON array with
some typical fields:

  $ dune trace cat --chrome-trace | jq 'type'
  "array"

Test that --sexp and --chrome-trace are mutually exclusive:

  $ dune trace cat --sexp --chrome-trace 2>&1 | head -1
  Error: --chrome-trace and --sexp are mutually exclusive
  [1]

Chrome trace timestamps must be in microseconds (not seconds) per the Chrome
Trace Event Format spec. A Unix timestamp in seconds (~1.7e9) vs microseconds
(~1.7e15) differs by 6 orders of magnitude; 1e12 sits safely between them:

  $ dune trace cat --chrome-trace | jq '.[0].ts > 1e12'
  true

Duration values should be whole-number microseconds (integer division from
nanoseconds):

  $ dune trace cat --chrome-trace | jq '[.[] | select(.dur) | .dur == (.dur | floor)] | all'
  true

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

Reject malformed event headers, duration pairs, argument fields, and process
argument lists. These are valid Csexp frames but not valid trace events.

  $ printf '%s' '(3:bad)' > input.csexp
  $ dune trace cat --trace-file input.csexp
  Error: invalid sexp
  (bad)
  [1]
  $ printf '%s' '(4:test4:data())' > input.csexp
  $ dune trace cat --trace-file input.csexp
  Error: invalid sexp
  ()
  [1]
  $ printf '%s' '(4:test4:data1:0(3:bad))' > input.csexp
  $ dune trace cat --trace-file input.csexp
  Error: invalid sexp
  (test data 0 (bad))
  [1]
  $ printf '%s' '(4:test4:data1:0(12:process_args(())))' > input.csexp
  $ dune trace cat --trace-file input.csexp
  Error: invalid sexp
  (test data 0 (process_args (())))
  [1]
