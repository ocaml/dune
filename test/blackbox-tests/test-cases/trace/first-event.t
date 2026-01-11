Demonstrate the first event that is emitted in the trace file. This must always
be a particular event emitted by dune.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ dune build

  $ dune trace cat | jq -s 'first | {name, cat, args: (.args | keys)}'
  {
    "name": "info",
    "cat": "log",
    "args": [
      "OCAMLPARAM",
      "message"
    ]
  }
