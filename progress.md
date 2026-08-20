# Priority Scheduler Implementation Progress

## Milestones

- [x] Oriented the branch, reviewed repository guidance, and confirmed that the
  existing priority scheduler is experimental and disabled by default
  ([plan.md:3-11](./plan.md#L3-L11),
  [plan.md:331-336](./plan.md#L331-L336)).
- [ ] Capture the mixed-root failure and add correlated scheduler tracing
  ([plan.md:297-329](./plan.md#L297-L329),
  [plan.md:338-362](./plan.md#L338-L362)).
- [ ] Add bidirectional queue priority updates while preserving FIFO and restart
  semantics ([plan.md:221-249](./plan.md#L221-L249),
  [plan.md:364-391](./plan.md#L364-L391)).
- [ ] Preserve resolved root grouping, infer `Direct`, `Normal`, and `Bulk`
  without changing the CLI, and add Action-builder demand scopes
  ([plan.md:39-65](./plan.md#L39-L65),
  [plan.md:107-137](./plan.md#L107-L137),
  [plan.md:393-421](./plan.md#L393-L421)).
- [ ] Replace scalar Memo priority with run-local, downward-only root demand
  propagation ([plan.md:139-219](./plan.md#L139-L219),
  [plan.md:423-458](./plan.md#L423-L458)).
- [ ] Remove demand on normal completion, cancellation, and watch restart
  ([plan.md:251-278](./plan.md#L251-L278),
  [plan.md:460-487](./plan.md#L460-L487)).
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
