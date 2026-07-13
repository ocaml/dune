Runtime trace events
====================

Runtime events are timing- and allocation-dependent, so only assert the stable
shape of emitted events.

  $ make_dune_project 3.22

  $ cat >dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (targets output.txt)
  >  (action (write-file output.txt ok)))
  > EOF

Use a small minor heap to make runtime GC events reproducible without asserting
exact event counts or counter values.

  $ unset OCAML_RUNTIME_EVENTS_DIR
  $ export OCAMLRUNPARAM=s=32k
  $ export DUNE_TRACE=runtime
  $ dune build @runtest

  $ dune trace cat | jq -s '
  >   [ .[] | select(.cat == "runtime") ]
  > | { has_runtime_events: (length > 0)
  >   , timestamps_in_trace_clock:
  >       all(.[].ts; type == "number" and . > 1000000000)
  >   , valid_shape:
  >       all(.[];
  >         if .name == "event" then
  >           ((.args.phase | type) == "string"
  >            and (.args.what == "begin" or .args.what == "end"))
  >         elif .name == "counter" then
  >           ((.args.name | type) == "string"
  >            and (.args.value | type) == "number")
  >         else
  >           false
  >         end)
  >   }
  > '
  {
    "has_runtime_events": true,
    "timestamps_in_trace_clock": true,
    "valid_shape": true
  }

The runtime events directory is an implementation detail, not part of the
process environment Dune reports in its init event.

  $ dune trace cat | jq -s '
  >   [ .[] | select(.cat == "config" and .name == "init") ][0].args.env
  > | any(startswith("OCAML_RUNTIME_EVENTS_DIR="))
  > '
  false
