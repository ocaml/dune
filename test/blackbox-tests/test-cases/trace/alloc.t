  $ cat > dune-project <<'EOF'
  > (lang dune 1.6)
  > EOF
  $ cat > dune <<'EOF'
  > (executable
  >  (name prog))
  > EOF
  $ cat > prog.ml <<'EOF'
  > let () = print_endline "hello, world"
  > EOF

The alloc sampler is only enabled when the alloc trace category is requested:

  $ export DUNE_TRACE="+alloc"
  $ dune build prog.exe
  $ dune trace cat | jq -s '
  >   [ .[]
  >   | select(.cat == "alloc")
  >   | { name
  >     , phase: .args.phase
  >     , has_run_id: (.args.run_id != null)
  >     , heaps:
  >         { minor: (.args.minor | keys)
  >         , major: (.args.major | keys)
  >         , promoted: (.args.promoted | keys)
  >         }
  >     , top_entries_are_traces:
  >         (all((.args.minor.top + .args.major.top + .args.promoted.top)[]?;
  >           ((keys | sort) == ["estimated_words", "samples", "trace"]
  >            and (.trace | type == "array")
  >            and (.trace | length <= 10)
  >            and all(.trace[]; type == "string"))))
  >     }
  >   ]'
  [
    {
      "name": "summary",
      "phase": "build",
      "has_run_id": true,
      "heaps": {
        "minor": [
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "top_entries_are_traces": true
    },
    {
      "name": "summary",
      "phase": "exit",
      "has_run_id": false,
      "heaps": {
        "minor": [
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "top_entries_are_traces": true
    }
  ]
