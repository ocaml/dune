The `sat` trace category emits a complete event for every SAT solve performed
while generating a lock directory. The events are opt-in: they are only
emitted when the `sat` category is enabled via `DUNE_TRACE=+sat`.

  $ mkrepo
  $ mkpkg foo 0.0.1
  $ mkpkg bar << EOF
  > depends: [ "foo" ]
  > EOF

With the `sat` category enabled, solving emits at least one `sat`/`solve`
event carrying the solver stats:

  $ export DUNE_TRACE=+sat
  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

Assert the shape of the `sat`/`solve` events. The exact counter values and the
number of events depend on the solver (it may run more than once, e.g. when
retrying with different `max_avoids`), so only assert sane ranges:

  $ dune trace cat | jq -s 'include "dune";
  >   [ .[] | satSolveEvents ] as $solves
  > | { has_sat_solve_events: ($solves | length > 0)
  >   , valid_shape: all($solves[];
  >       (.args.num_variables | type) == "number"
  >       and (.args.num_clauses | type) == "number"
  >       and (.args.num_decisions | type) == "number"
  >       and (.args.num_conflicts | type) == "number"
  >       and .args.num_variables >= 1
  >       and .args.num_clauses >= 1
  >       and .args.num_decisions >= 0
  >       and .args.num_conflicts >= 0
  >       and (.dur | type) == "number")
  >   }
  > '
  {
    "has_sat_solve_events": true,
    "valid_shape": true
  }

The `sat` category is opt-in: without `DUNE_TRACE=+sat`, no `sat` events are
emitted at all:

  $ unset DUNE_TRACE
  $ rm -rf "${source_lock_dir}"
  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ dune trace cat | jq -s '[ .[] | select(.cat == "sat") ] | length'
  0
