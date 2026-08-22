# Class-Only Root Demand Scheduler Plan

## Status

This plan replaces the history-dependent priority propagation on the current
branch with run-scoped root demand classes. It does not add a command-line
option or otherwise change the user-facing build interface.

The first implementation slice is intentionally limited to the scheduling
policy needed by the observed mixed-root workload. More speculative heuristics
remain deferred until this slice is validated against that workload.

## Objective

For a build such as:

```text
dune build fmdeps/auto send.vo
```

classify the recursive directory request as bulk work and the concrete file
request as direct work. Propagate direct demand to every prerequisite needed to
unlock `send.vo`, including already-active and dynamically discovered
prerequisites. Once such work is ready, schedule it ahead of work required only
by the recursive request.

The change must:

- preserve build correctness, Memo sharing, rule identity, and action digests;
- avoid history-dependent priority inflation;
- avoid leaking demand from a shared dependency into unrelated callers;
- remove stale demand after cancellation and watch-mode restart;
- preserve FIFO ordering between jobs with the same demand class;
- leave priority scheduling disabled by default while it remains experimental;
- execute the same required action set as the non-priority scheduler.

## Decisions

### No new user interface

Do not add `--priority-target` or interpret command-line ordering as urgency.
Infer demand from target semantics that Dune already has after resolution:

| Resolved request | Demand class |
| --- | --- |
| Concrete file or generated directory target | `Direct` |
| Ordinary, non-recursive alias | `Normal` |
| Recursive alias or source directory expanded to one | `Bulk` |

A source directory initially enters target resolution as a file-like command
line argument, but `bin/target.ml` converts it to a recursive default alias.
Classification must therefore use the resolved request while preserving its
original root grouping.

If one original target expands to multiple build contexts, all corresponding
requests remain under the same root demand. Target resolution should assert or
handle explicitly any case where one original target unexpectedly resolves to
requests with different classes.

Non-CLI build entry points wrap their requests in a synthetic `Normal` demand
scope. Do not add an unconditional normal scope around every
`Build_system.Request.Goal`: a CLI goal already contains direct, normal, and
bulk root scopes, and an outer normal root would incorrectly promote all bulk
work. Scheduler-internal work that has no build root uses an explicit internal
normal priority rather than relying on an empty demand set.

### Class-only scheduling in the first slice

Use the following finite ordering:

```ocaml
type demand_class =
  | Undemanded
  | Bulk
  | Normal
  | Direct
```

```text
Direct > Normal > Bulk > Undemanded > FIFO
```

`Undemanded` is an internal state needed when the last root is removed from a
queued computation. It must not silently become `Normal`.

Do not include dependency depth, fan-out, blocked-dependent count, lane
reservation, process preemption, or historical duration in this slice. In
particular, do not preserve the current behavior where repeated consumers raise
an integer priority. That behavior is part of the observed failure mode, not a
stable scheduler contract.

### Scheduling metadata is not correctness metadata

Demand class and root identity must not be included in:

- Memo inputs or cache keys;
- rule identity;
- action digests;
- dependency facts;
- local or shared build-cache keys.

A stronger root must update the existing shared computation rather than create
another Memo node or execute a rule a second time.

## Architecture

### Root grouping and Action-builder scope

Change target resolution to retain one group per original target instead of
flattening all resolved requests immediately. A conceptual result type is:

```ocaml
type resolved_root =
  { requests : Request.t list
  ; demand_class : demand_class
  ; description : string
  }
```

The description is for tracing only. It must not participate in equality or
build correctness.

Construct one Action-builder branch for each resolved root and wrap the whole
branch in an outer, non-memoized demand scope:

```ocaml
Action_builder.with_job_demand demand_class root_request
```

The wrapper allocates a fresh root ID when the branch is evaluated eagerly in a
Memo run. It must not bake a run-specific ID into a sticky Action-builder that
can be evaluated again after a watch restart. Lazy dependency collection does
not need to register scheduling demand.

`Action_builder.Seq`, `Both`, and `All` may continue evaluating branches
concurrently. The demand scope, rather than evaluation order, distinguishes the
roots.

### Run-local demand registry

Replace persistent `Dep_node.job_priority` state with a side table owned by the
current Memo run. The exact representation can vary, but it must provide these
relationships:

```ocaml
type root_id

type root =
  { id : root_id
  ; class_ : demand_class
  ; generation : Memo.Run.t
  }

type node_demand =
  { node : Dep_node.packed
  ; mutable roots : demand_class Root_id.Map.t
  ; mutable score : demand_class
  ; mutable queue_handle : Fiber.Throttle.priority option
  }

type run_registry =
  { generation : Memo.Run.t
  ; nodes : node_demand Dep_node.Id.Table.t
  ; touched_nodes : Dep_node.Id.Set.t Root_id.Table.t
  }
```

The registry is run-scoped even though the priority factory currently spans a
scheduler invocation containing multiple watch runs. On a change to
`Memo.Run.current ()`, create an empty registry. Assert or trace that the old
registry has been cancelled or otherwise made quiescent before replacing it.

Persistent Memo nodes may retain stable IDs, but they must not retain a demand
class that seeds a later run.

### Demand propagation

Use ambient Fiber state only to enter a root scope. Node registry state is the
authoritative source for nested and dynamic dependencies.

When executing a Memo dependency:

1. If there is no Memo caller, add the ambient root to the dependency.
2. If there is a caller, copy every root currently attached to the caller into
   the dependency.
3. Record the active caller-to-dependency relationship as the current branch
   already does for nested promotion.
4. If a new root reaches an already-active node, propagate that root
   iteratively through all of the node's active dependencies.
5. If a dynamic dependency is discovered later, copy the caller node's current
   roots when that dependency is entered.
6. Do not update the caller when a dependency returns.

The core operation is conceptually:

```ocaml
let add_root registry node root =
  if node_already_has_root registry node root.id
  then ()
  else begin
    record_root registry node root;
    recompute_and_set_queue_priority registry node;
    active_dependencies node
    |> List.iter ~f:(fun dependency -> add_root registry dependency root)
  end
```

Implement this with an explicit worklist rather than recursive OCaml calls.
Each `(root_id, node_id)` pair is processed at most once, so propagation is
idempotent and terminates even while Memo is examining a transient dependency
cycle.

Adding a second root must propagate even when it has the same class as the
first root and therefore does not change the node's aggregate queue score. Root
membership, not aggregate score change, controls transitive propagation.

There is deliberately no return inheritance. A caller already has the roots
that require its result. A bulk caller waiting for a dependency that is also
needed by a direct root therefore remains bulk after the dependency completes.

### Queue priority updates

Keep integer queue priorities for the first slice:

```text
Undemanded = 0
Bulk       = 1
Normal     = 2
Direct     = 3
```

Add `set_priority` to `Fiber.Priority_queue` and `Fiber.Throttle`. It must:

- update queued handles in either direction;
- retain each waiter's original enqueue sequence;
- reinsert a queued handle using the sequence of its current head waiter;
- leave an unqueued handle ready to use its new value on the next push;
- reject a priority handle owned by another queue as today.

Keep FIFO sequence outside the mutable semantic priority. A priority update
must not make a job artificially younger or older.

`Throttle.restart_after_job` must continue reserving a continuation only when
its semantic priority is strictly greater than the highest waiting priority.
Equal-priority work must release the slot to normal FIFO selection.

Do not generalize the queue to a Dune-specific structured score yet. If later
work needs more than a finite class, generalize the queue behind a comparator
or ordered-priority abstraction rather than coupling `Fiber` to build concepts.

### Root removal and cancellation

Normal completion and cancellation need separate cleanup paths.

For normal completion, finalize every eagerly evaluated demand scope. Removing
a root:

1. iterates the root's touched-node set;
2. removes the root from each node's root map;
3. recomputes the node's strongest remaining class;
4. lowers any queued priority handle when required;
5. is idempotent.

A root finalizer is not sufficient for build cancellation. A process waiting in
`Scheduler.with_job_slot` checks cancellation only after it is admitted, so its
branch may not unwind promptly. Wrap the build request in a
`Fiber.Cancel.with_handler`, or hook the equivalent existing build-cancellation
path, so firing `Process.Build.cancellation` invalidates all roots in the
current registry immediately.

Global cancellation may invalidate the whole registry instead of removing each
root individually. This operation must be idempotent and must happen before a
watch restart creates demand for the next Memo run.

Queued cancelled process attempts are not removed from the current throttle.
Demoting them prevents stale direct demand from affecting ordering, but they may
still be admitted later and then observe cancellation. Treat queue removal as a
separate improvement only if cancellation drain time remains significant.

### Shared rules, multiple targets, and dynamic dependencies

Demand propagation must preserve existing synchronization points:

- `build_file` and `build_alias` retain their current Memo keys;
- multiple targets produced by one rule continue to converge on one
  `execute_rule` Memo node;
- file selectors and directory targets propagate through their existing Memo
  calls;
- anonymous actions retain their current synchronization behavior;
- `Action_exec` dynamic dependency stages inherit all roots currently attached
  to the executing caller node.

A direct request arriving after a shared rule started under bulk demand must
promote that rule and its still-active nested dependencies without starting a
second execution.

## Trace instrumentation

Split instrumentation into two levels.

Baseline queue instrumentation can be added before root plumbing:

- stable queue entry or process-attempt ID;
- integer priority at enqueue and dequeue;
- enqueue, ready, and start timestamps;
- queue length;
- selected priority and highest waiting priority;
- restart reservation and release decisions.

After root plumbing, add Memo/build-level fields:

- Memo run generation;
- root ID, resolved-root description, and class;
- Memo node ID and name;
- rule or target identity when available;
- old and new node class;
- reason for an update: root entry, late shared demand, dynamic dependency,
  root removal, cancellation, or run reset.

Correlate scheduler events with existing process/rule trace identity. Do not
make the generic Fiber queue interpret Memo nodes or rule targets.

Instrumentation must make it possible to answer:

- Which root caused this process to be `Direct`?
- When did the process become ready?
- What was selected instead while it waited?
- Did a direct class reach every source generator needed by the Rocq barrier?
- Did a direct class propagate back into a bulk-only continuation?

## Implementation and commit sequence

Keep regression tests in commits preceding their fixes where possible. Every
commit must pass the repository checks. If an API test cannot compile before
the API exists, keep the API and its direct unit tests together while retaining
behavioral regression tests in an earlier commit.

### Stage 1: Reproduce and instrument the scheduling failure

Likely files:

- `src/dune_scheduler/event.ml`
- `src/dune_scheduler/event.mli`
- `src/dune_scheduler/scheduler.ml`
- `src/memo/node.ml`
- `bench/priority-scheduler/`
- `bench/priority-scheduler-rocq/`

Work:

1. Add baseline queue timing and selection trace events.
2. Add a deterministic mixed-root workload with:
   - one recursive bulk root;
   - one concrete direct target;
   - generated source prerequisites;
   - a fan-in barrier analogous to the Rocq dependency rule;
   - enough bulk jobs to reproduce continual bulk selection.
3. Capture the current failure without relying only on wall-clock timing.
4. Record the action multiset for later equivalence checks.

The existing single-alias benchmarks remain useful for measuring the old
heuristic, but they are not merge gates for the class-only scheduler.

### Stage 2: Add bidirectional queue updates

Likely files:

- `src/fiber/src/priority_queue.ml`
- `src/fiber/src/priority_queue.mli`
- `src/fiber/src/throttle.ml`
- `src/fiber/src/fiber.mli`
- `src/fiber/test/fiber_tests.ml`

Tests:

- lowering a queued priority changes selection;
- raising a queued priority still changes selection;
- reprioritization preserves FIFO sequence;
- lowering one shared handle preserves FIFO among its waiters;
- resize admits the highest current priority;
- demotion before restart processing does not strand a waiter;
- restart blockers and reservations remain balanced;
- equal priorities do not retain a reserved slot ahead of older FIFO work.

Implementation:

- add `Priority_queue.set_priority`;
- expose the corresponding `Throttle.set_priority`;
- implement existing increment operations in terms of `set_priority` if they
  remain necessary;
- preserve the disabled scheduler path unchanged.

### Stage 3: Preserve root grouping and add demand scope

Likely files:

- `bin/target.ml`
- `src/dune_engine/action_builder.ml`
- `src/dune_engine/action_builder.mli`
- associated target and Action-builder tests

Tests:

- a concrete file resolves as `Direct`;
- a generated directory target resolves as `Direct`;
- an ordinary alias resolves as `Normal`;
- a recursive alias resolves as `Bulk`;
- a source directory converted to a recursive default alias resolves as
  `Bulk`;
- context expansion preserves one root grouping and class;
- command-line target order has no effect on class;
- no new command-line option is accepted or required.

Implementation:

- retain resolved requests grouped by original target;
- classify the resolved group;
- add the outer, non-memoized Action-builder demand scope;
- allocate root IDs during eager evaluation, not target parsing;
- initially trace scopes without changing queue policy if that keeps this stage
  behavior-preserving.

### Stage 4: Replace scalar Memo priority with run-local class propagation

Likely files:

- `src/memo/node.ml`
- `src/memo/node.mli`
- `src/memo/exec.ml`
- `src/memo/memo.ml`
- `src/memo/memo.mli`
- `src/dune_scheduler/scheduler.ml`
- `test/expect-tests/scheduler_tests.ml`

Regression tests before the fix:

- observing the same root-to-dependency path twice is idempotent;
- a second root of the same class still reaches active nested dependencies;
- late `Direct` demand promotes an already-queued shared computation;
- a shared dependency merges `Bulk` and `Direct` roots;
- the shared dependency's `Direct` class does not leak into its bulk caller;
- dynamic dependencies inherit roots attached after rule execution started;
- a transient Memo cycle terminates and reports the existing cycle error;
- multiple targets of one rule still execute the rule once.

Implementation:

- create the run-local registry;
- seed roots at demand scopes;
- propagate root IDs only downward with an explicit worklist;
- update queue handles through `set_priority`;
- delete aggregate return inheritance;
- stop using persistent node priority values to initialize later runs;
- keep the existing priority-scheduling opt-in as the behavior boundary.

The current tests where two same-class consumers outrank one same-class
consumer must be replaced or reclassified. The first slice intentionally uses
FIFO for that tie.

### Stage 5: Implement lifecycle cleanup

Likely files:

- `src/dune_engine/build_system.ml`
- `src/dune_engine/build_loop.ml`
- `src/dune_engine/process.ml` if the cancellation hook belongs there
- `src/memo/node.ml`
- scheduler and Memo expect tests

Tests:

- removing `Direct` from a shared node reveals remaining `Bulk` demand;
- removing a root twice is harmless;
- normal root completion clears touched-node membership;
- build cancellation demotes queued direct work before branch finalizers run;
- watch restart starts with an empty registry;
- old queue handles cannot seed a new Memo run;
- cancellation with a blocked restart does not leak a reservation;
- `-j1` continues making progress without a lane-reservation policy;
- scheduler resize remains correct during cancellation and restart.

Implementation:

- finalize normal root scopes;
- invalidate the registry through the build cancellation handler;
- lazily create a registry for each Memo run generation;
- add invariants and trace events around old-run quiescence.

### Stage 6: Validate the complete vertical slice

Run the focused tests first, then the required repository checks. Do not run
Dune commands concurrently.

Suggested progression:

```text
dune runtest src/fiber/test/
dune runtest test/expect-tests/scheduler_tests.ml
dune runtest <new mixed-root test or benchmark target>
dune build @check
dune fmt
dune build @check @fmt @runtest
```

Use the fully built Dune binary for manual workload trials, not the stale
bootstrap binary behind `./dune.exe`. Build it through the normal development
flow; do not run `make bootstrap` without user approval. Ask before promoting
changed test output.

For the real workload, run clean enabled/disabled A/B trials in balanced order
and capture traces. Define numeric target-latency and total-makespan thresholds
with the workload owner before using them as acceptance gates.

## Acceptance criteria

### Deterministic behavior

- Repeating a root-to-node observation does not change its score.
- A different root propagates even when its class equals the current maximum.
- `Direct` dominates any amount of bulk traversal or repeated observation.
- Shared work contains both direct and bulk roots without duplicating execution.
- Direct demand never returns into a bulk-only caller.
- Removing or cancelling direct demand can lower queued work to bulk.
- Watch restart begins without priority state from the previous Memo run.
- Equal-class jobs retain FIFO ordering.
- Memo cycles, dynamic dependencies, selectors, aliases, directory targets,
  anonymous actions, and multi-target rules preserve existing correctness.
- Priority scheduling disabled behaves as on `origin/main` with respect to
  queue admission and action selection.

### Mixed-root and real workload

- Every `cpp2v` action needed by the vSwitch Rocq dependency barrier is traced
  as `Direct` before queue selection.
- Hammer-only actions remain `Bulk`.
- No bulk process attempt is selected while a direct process attempt is ready.
- The delayed source generators no longer experience the observed long queue
  wait caused by continual hammer selection.
- The vSwitch `rocq dep` action starts substantially earlier.
- `send.vo` completion satisfies the agreed improvement threshold.
- Total makespan and CPU utilization satisfy the agreed regression threshold.
- Enabled and disabled schedules execute the same required action multiset.

## Expected size

From the current branch, expect approximately:

- 400-700 lines of production-code churn;
- 400-700 lines of tests, tracing, and workload support;
- 10-15 touched files;
- 6-10 logical commits, depending on how regression tests can be separated
  while keeping every commit buildable.

Much of the current scalar priority propagation will be replaced rather than
augmented, so net additions should be smaller than total diff churn.

## Risks and mitigations

### Root-to-node state growth

The registry can grow with the product of active roots and reached nodes.
Measure root/node pair counts and registry memory. Keep the state run-local and
avoid registering current cached nodes that have no pending work.

### Delayed cancellation drain

Demotion removes stale urgency but does not remove cancelled waiters from the
throttle. Trace cancellation drain time. Add queue removal only if it is a
measured problem.

### Regression of the current single-alias benchmark

Class-only scheduling intentionally removes same-class fan-out inflation. Treat
a regression in that synthetic benchmark as a documented trade-off. The merge
gate is the mixed-root workload plus an agreed total-makespan bound.

### All slots already occupied

Class ordering affects ready jobs but does not preempt running bulk processes.
A direct job may still wait for one running job to finish. Measure this before
adding a lane reservation; do not add a cap speculatively.

### Rocq dependency barrier remains real

The scheduler can prioritize all generators required by the local and
dependent-theory source barriers, but it cannot compile `send.vo` before the
current dependency map exists. Any rule sharding or historical-map work remains
a separate Rocq change.

## Deferred work

Do not include the following in the initial class-only change:

- blocked-dependent or fan-out scoring;
- structured critical-path scores;
- persistent action graphs or durations;
- bulk-slot caps or class-aware concurrency reservation;
- process preemption;
- Rocq dependency-rule sharding;
- a new command-line option.

After the real-workload gate, add one heuristic at a time only when traces show
a remaining scheduling failure. Any blocked-dependent metric must have explicit
edge identity, exact registration/removal semantics, and bidirectional queue
updates. Historical data may influence ordering only and must never affect
build correctness.
