# Priority Scheduler Implementation Progress

## Milestones

- [x] Oriented the branch, reviewed repository guidance, and confirmed that the
  existing priority scheduler is experimental and disabled by default
  ([plan.md:3-11](./plan.md#L3-L11),
  [plan.md:331-336](./plan.md#L331-L336)).
- [ ] Capture the mixed-root failure and add correlated scheduler tracing
  ([plan.md:297-329](./plan.md#L297-L329),
  [plan.md:338-362](./plan.md#L338-L362)).
- [x] Add bidirectional queue priority updates while preserving FIFO and restart
  semantics ([plan.md:221-249](./plan.md#L221-L249),
  [plan.md:364-391](./plan.md#L364-L391)).
- [ ] Preserve resolved root grouping, infer `Direct`, `Normal`, and `Bulk`
  without changing the CLI, and add Action-builder demand scopes
  ([plan.md:39-65](./plan.md#L39-L65),
  [plan.md:107-137](./plan.md#L107-L137),
  [plan.md:393-421](./plan.md#L393-L421)).
- [ ] Replace scalar Memo priority with run-local, downward-only root demand
  propagation; implementation is present, but this milestone remains open until
  generation-transition quiescence and the planned dynamic dependency, cycle,
  and multi-target integration coverage are complete
  ([plan.md:139-219](./plan.md#L139-L219),
  [plan.md:423-487](./plan.md#L423-L487),
  [plan.md:516-529](./plan.md#L516-L529)).
- [ ] Remove demand on normal completion, cancellation, and watch restart;
  lifecycle implementation and focused coverage are present, but production
  watch/RPC restart integration remains for Task 9
  ([plan.md:251-278](./plan.md#L251-L278),
  [plan.md:460-487](./plan.md#L460-L487),
  [plan.md:516-529](./plan.md#L516-L529)).
- [ ] Validate deterministic behavior, the mixed-root workload, and the full
  repository checks ([plan.md:489-542](./plan.md#L489-L542)).

## Progress Log

- **2026-08-20:** Confirmed the implementation starts from commit `e349172d23`
  and that unrelated local changes in `src/dune_rules/install_layout.ml` and
  `recommendations.md` must remain untouched
  ([plan.md:92-103](./plan.md#L92-L103)).
- **2026-08-20:** Confirmed that the fully built Dune binary is available under
  `_build/install/default/bin/dune`; manual scheduler validation will not use
  the stale bootstrap binary ([plan.md:489-512](./plan.md#L489-L512)).
- **2026-08-20:** Established a green baseline with
  `./dune.exe runtest src/fiber/test/`,
  `./dune.exe runtest test/expect-tests/scheduler_tests.ml`, and
  `./dune.exe build @check`
  ([plan.md:489-512](./plan.md#L489-L512)).
- **2026-08-20:** Mapped the implementation seams: queue demotion can reuse the
  existing remove/reinsert path; root grouping is lost only in
  `resolve_targets_exn`; eager Action-builder evaluation is the safe root-ID
  allocation boundary; and cancellation precedes `Memo.reset`
  ([plan.md:107-176](./plan.md#L107-L176),
  [plan.md:221-278](./plan.md#L221-L278)).
- **2026-08-20:** Added absolute queue and throttle priority updates with tests
  for demotion, FIFO age, shared handles, resize, equal-priority admission, and
  deferred and blocked restart balance. Formatted the five changed OCaml files
  with `ocamlformat -i`; `./dune.exe runtest src/fiber/test/` and
  `./dune.exe build @check` passed
  ([plan.md:221-249](./plan.md#L221-L249),
  [plan.md:364-391](./plan.md#L364-L391)).
- **2026-08-20:** Preserved one group per original target, classified resolved
  file targets as `Direct`, non-recursive aliases as `Normal`, and recursive
  aliases as `Bulk`, and added an eager-only Action-builder demand scope that
  allocates fresh roots at evaluation time. `./dune.exe build @check @fmt`
  passed ([plan.md:39-65](./plan.md#L39-L65),
  [plan.md:107-137](./plan.md#L107-L137),
  [plan.md:393-421](./plan.md#L393-L421)).
- **2026-08-20:** Added a minimal test-only root observation and expect coverage
  proving that lazy Action-builder evaluation allocates no root and repeated
  eager evaluations allocate distinct roots carrying the requested class.
  Classification and target action-set integration coverage remains for Stage
  4/6, so the Stage 3 milestone remains open
  ([plan.md:107-137](./plan.md#L107-L137),
  [plan.md:393-421](./plan.md#L393-L421),
  [plan.md:423-451](./plan.md#L423-L451),
  [plan.md:516-542](./plan.md#L516-L542)).
- **2026-08-20:** Replaced persistent scalar node priorities with a side-table
  registry keyed by scheduler factory and Memo generation. Root membership now
  propagates only from callers to active dependencies through an idempotent
  `(root, node)` worklist, and queue handles use the maximum class score without
  dependency-to-caller inheritance. Generation-transition demotion and
  quiescence remain Stage 5 lifecycle work
  ([plan.md:139-219](./plan.md#L139-L219),
  [plan.md:423-487](./plan.md#L423-L487)).
- **2026-08-20:** Replaced repeated-consumer expectations with class-based tests
  covering repeated observation of one root, distinct same-class roots, late
  nested `Direct` promotion, exact `Direct`/`Bulk` membership without upward
  leakage, restoration, asynchronous continuation priority, and a direct
  dependency chain. Target classification, dynamic dependency, transient
  cycle, and multi-target rule integration remain for Stage 6/Task 9.
  `./dune.exe runtest test/expect-tests/scheduler_tests.ml`,
  `./dune.exe runtest src/fiber/test/ test/expect-tests/memo/`, and
  `./dune.exe build @check @fmt` passed
  ([plan.md:435-458](./plan.md#L435-L458),
  [plan.md:489-529](./plan.md#L489-L529)).
- **2026-08-20:** Added idempotent root finalization and registry invalidation.
  Root removal uses touched-node membership to recompute and lower aggregate
  scores; `Process.Build.cancel` demotes all handles synchronously before firing
  cancellation, including action-runner worker cancellation; and factory or
  Memo-generation transitions invalidate the old registry before creating an
  empty one. Invalidation is terminal within the same factory and generation.
  Non-CLI `Build_system.run` work receives a synthetic `Normal` scope without
  wrapping CLI goals
  ([plan.md:251-278](./plan.md#L251-L278),
  [plan.md:460-487](./plan.md#L460-L487)).
- **2026-08-20:** Added lifecycle expectations proving normal cleanup and
  repeated removal from a real Memo node and queue handle, `Direct` removal
  revealing `Bulk`, cancellation demotion of queued work before branch
  finalizers, terminal same-run invalidation, same-factory Memo-generation
  isolation, and enabled/disabled synthetic `Build_system.run` demand. Existing
  Fiber coverage continues to exercise deferred restart, resize,
  equal-priority, and shared-handle accounting; scheduler lifecycle cases run at
  `-j1`. Production watch/RPC restart integration remains for Task 9, so the
  lifecycle milestone remains open
  ([plan.md:460-487](./plan.md#L460-L487),
  [plan.md:489-529](./plan.md#L489-L529)).
