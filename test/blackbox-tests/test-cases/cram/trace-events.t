Demonstrate teh trace events emitted for running a cram test:


  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat >dune <<EOF
  > (cram (shell bash))
  > EOF

  $ cat >test.t <<EOF
  >   $ true
  > EOF

  $ dune runtest test.t

  $ dune trace cat | jq 'select(.cat == "cram") | .args | .commands[0] |= keys'
  {
    "test": "_build/default/test.t",
    "commands": [
      [
        "command",
        "real",
        "system",
        "user"
      ]
    ]
  }

  $ dune trace cat | jq '
  >   select(.cat == "process" and (.args.categories | index("cram")) and .name == "finish")
  > | .args
  > | { fields: keys, name }
  > '
  {
    "fields": [
      "categories",
      "dir",
      "exit",
      "name",
      "pid",
      "process_args",
      "prog",
      "rusage"
    ],
    "name": "test.t"
  }
