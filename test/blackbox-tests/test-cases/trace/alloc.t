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
  >   | [ .args.minor, .args.major, .args.promoted ] as $heaps
  >   | { name
  >     , phase: .args.phase
  >     , has_run_id: (.args.run_id != null)
  >     , config: .args.config
  >     , exact_allocation_counters:
  >         ((.args.exact | type) == "object"
  >          and ((.args.exact | keys | sort)
  >               == ["major_words", "minor_words", "promoted_words"])
  >          and all(.args.exact[]; type == "number" and . >= 0)
  >          and (if .args.phase == "build"
  >               then .args.exact.minor_words > 0
  >               else true
  >               end))
  >     , heaps:
  >         { minor: (.args.minor | keys)
  >         , major: (.args.major | keys)
  >         , promoted: (.args.promoted | keys)
  >         }
  >     , entries_have_sources_and_traces:
  >         all($heaps[];
  >           (.top | length) <= 2
  >           and all(.top[]?;
  >             ((keys | sort) == ["estimated_words", "samples", "source", "trace"]
  >              and (.source | type == "string")
  >              and (.trace | type == "array")
  >              and (.trace | length <= 3)
  >              and all(.trace[]; type == "string")))
  >           and all(.by_source[]?;
  >             ((keys | sort) == ["estimated_words", "samples", "source"]
  >              and (.source | type == "string"))))
  >     , frame_entries_have_sources_and_locations:
  >         all($heaps[];
  >           (.by_site | type) == "array"
  >           and (.by_frame | type) == "array"
  >           and (.by_site | length) <= 2
  >           and (.by_frame | length) <= 2
  >           and all((.by_site + .by_frame)[]?;
  >             ((keys | sort) == ["estimated_words", "frame", "samples", "source"]
  >              and (.source | type == "string")
  >              and (.frame | type == "string"))))
  >     , rankings_are_sorted:
  >         all($heaps[];
  >           all([.top, .by_site, .by_frame][];
  >             [.[].samples] as $samples
  >             | $samples == ($samples | sort | reverse)))
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
      "exact_allocation_counters": true,
      "heaps": {
        "minor": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "entries_have_sources_and_traces": true,
      "frame_entries_have_sources_and_locations": true,
      "rankings_are_sorted": true
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
      "exact_allocation_counters": true,
      "heaps": {
        "minor": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "major": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ],
        "promoted": [
          "by_frame",
          "by_site",
          "by_source",
          "top",
          "total_samples",
          "total_words"
        ]
      },
      "entries_have_sources_and_traces": true,
      "frame_entries_have_sources_and_locations": true,
      "rankings_are_sorted": true
    }
  ]

Invalid profiler settings are rejected when allocation tracing is enabled:

  $ DUNE_TRACE_ALLOC="rate=2" dune build prog.exe
  Error: invalid DUNE_TRACE_ALLOC value "rate=2": rate must be greater than 0
  and at most 1
  [1]
