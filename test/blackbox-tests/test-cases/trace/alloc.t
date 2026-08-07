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
  $ export DUNE_TRACE_ALLOC="rate=0.001,stack=3,top=2"
  $ dune build prog.exe
  $ dune trace cat | jq -s '
  >   [ .[]
  >   | select(.cat == "alloc")
  >   | { name
  >     , phase: .args.phase
  >     , has_run_id: (.args.run_id != null)
  >     , config: .args.config
  >     , heaps:
  >         { minor: (.args.minor | keys)
  >         , major: (.args.major | keys)
  >         , promoted: (.args.promoted | keys)
  >         }
  >     , entries_have_sources_and_traces:
  >         ((.args.minor.top | length) <= 2
  >          and (.args.major.top | length) <= 2
  >          and (.args.promoted.top | length) <= 2
  >          and all((.args.minor.top + .args.major.top + .args.promoted.top)[]?;
  >           ((keys | sort) == ["estimated_words", "samples", "source", "trace"]
  >            and (.source | type == "string")
  >            and (.trace | type == "array")
  >            and (.trace | length <= 3)
  >            and all(.trace[]; type == "string")))
  >          and all((.args.minor.by_source
  >                   + .args.major.by_source
  >                   + .args.promoted.by_source)[]?;
  >            ((keys | sort) == ["estimated_words", "samples", "source"]
  >             and (.source | type == "string"))))
  >     }
  >   ]'
  [
    {
      "name": "summary",
      "phase": "build",
      "has_run_id": true,
      "config": {
        "sampling_rate": 0.001,
        "callstack_size": 3,
        "top_entry_count": 2
      },
      "heaps": {
        "minor": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "entries_have_sources_and_traces": true
    },
    {
      "name": "summary",
      "phase": "exit",
      "has_run_id": false,
      "config": {
        "sampling_rate": 0.001,
        "callstack_size": 3,
        "top_entry_count": 2
      },
      "heaps": {
        "minor": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "entries_have_sources_and_traces": true
    }
  ]

Invalid profiler settings are rejected when allocation tracing is enabled:

  $ DUNE_TRACE_ALLOC="rate=2" dune build prog.exe
  Error: invalid DUNE_TRACE_ALLOC value "rate=2": rate must be greater than 0
  and at most 1
  [1]
