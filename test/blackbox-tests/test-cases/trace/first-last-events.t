Demonstrate the first event that is emitted in the trace file. This must always
be a particular event emitted by dune.

  $ make_dune_project 3.22

  $ dune build

  $ dune trace cat | jq -s 'first | {name, cat, args: (.args | keys)}'
  {
    "name": "init",
    "cat": "config",
    "args": [
      "argv",
      "build_dir",
      "env",
      "initial_cwd",
      "pid",
      "root",
      "start"
    ]
  }

Demonstrate the last event:

  $ dune trace cat | jq -s '
  >   last
  > | { name, cat, args: (.args | .gc |= keys | .io |= keys | .digest |= keys) }
  > '
  {
    "name": "exit",
    "cat": "config",
    "args": {
      "gc": [
        "compactions",
        "heap_words",
        "major_collections",
        "major_words",
        "minor_collections",
        "minor_words",
        "promoted_words",
        "stack_size",
        "top_heap_words"
      ],
      "io": [
        "directories_read",
        "files_read",
        "files_written"
      ],
      "digest": [
        "files",
        "values"
      ]
    }
  }
