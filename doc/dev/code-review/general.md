# Code review — general guidance

How to review dune PRs. Applies to every PR; per-kind specifics live in
sibling files (`feat.md`, `fix.md`, `refactor.md`, `test.md`, `docs.md`),
and the OCaml style guide is in `code-style.md`.

## Read the stated intent first

Before reading the diff, understand what the author says the change does.
Sources, in order:

1. **PR description** — what the author wrote up when opening the PR.
2. **Commit message body** — often has rationale beyond the subject line.
3. **Linked issue** — original problem statement.
4. **`doc/changes/`** — a new entry signals a user-facing change.

Extract:

- **The problem** being solved (the "why").

- **The chosen approach** (the "how").

- **Caveats / known limitations** the author flags.

- **Scope boundaries** — what the PR explicitly is *not* doing.

If intent is missing or vague, ask the author to clarify before reviewing.

## Classify the PR

The kind of change shapes what to look for. Identify from the commit prefix,
PR title, or diff:

- **`feat:`** (or `feature:`) — new feature or user-facing capability

- **`fix:`** — bug fix

- **`refactor:`** — internal restructuring with no behaviour change

- **`test:`** — tests only

- **`docs:`** (or `doc:`) — documentation

`chore:` is a *label*, not a kind. Read the diff and dispatch on what the
chore actually does: a dep bump, a CI/build tweak, an internal cleanup, a
doc edit, or a feature removal mis-labelled as chore. Route to the
matching kind file (or skip the per-kind step for a trivial dep bump).
Two chore-flavour concerns don't fit any kind:

- **Dep-bump blast radius.** Does the bump pull new transitive
  constraints? Is the bumped tool already covered transitively?

- **Feature removal filed as `chore:` is mis-classified** — treat as
  `feat:`/`changed:`/`BREAKING CHANGE:` and ensure release-notes framing.

Two prefix patterns to watch for:

- **Subsystem prefixes** like `rpc:`, `engine:`, `pkg:`, `fiber:`, `trace:`,
  `cram:`, `scheduler:`, `debug:`, `build:`, `infra:`, `scaffold:` name the
  *scope*, not the kind. Look at the description and diff to decide which
  kind it really is (usually `feat:`, `fix:`, or `refactor:`).

- **No prefix** is common (~28% of PRs). Infer the kind from the title's
  verb and the diff: *Fix X* → `fix:`; *Add X*, *Trace X*, *Track X* →
  `feat:`; *Move X*, *Clean up X*, *Simplify X*, *Pass X* → `refactor:`;
  *Revert X* → usually `fix:` (find out *why* the revert is needed).

Backports (titled `[3.X.Y] backport #NNNN`) are usually `fix:` applied to
a release branch — see `fix.md`'s backport note.

Flag mismatch between the declared kind and the diff — a `fix:` PR that
also refactors, or a `feat:` PR that fixes unrelated bugs, should be
split. After classifying, also read the matching kind file in this
directory.

## The three questions

Every review answers three questions:

1. **Will this break anything?** Edge cases, regressions, error handling.
2. **Is the implementation reasonable?** Right approach, maintainable, no
   irrelevant changes riding along.
3. **Is it adequately tested?** Coverage, edge cases, meaningful tests.

## Tier every issue

Each comment is one of three tiers. Pick deliberately — a flat 10-bullet
review with no priority signal is useless to the author.

- **blocker** — the PR cannot merge as-is. Correctness bugs, missing
  invariant guards that surface as cryptic errors, undocumented semantic
  changes to a public API, missing changelog for a user-visible change.

- **must-fix** — needed before merge but the author has discretion on the
  shape of the fix. Missing tests for new behaviour, stale `.mli`
  docstrings, `assert` where `Code_error.raise` belongs.

- **nit** — optional. Style polish, naming preference, dead parameters,
  eta-wrappers, comment requests for non-load-bearing invariants. Author
  can take or leave.

Mark each inline comment with `**blocker**`, `**must-fix**`, or `**nit**`
at the start of the comment. The summary opens with the blocker list, then
must-fix; nits are not mentioned in the summary.

If you have zero blockers, say so explicitly in the summary ("no blockers,
N must-fix, M nits"). That tells the author the shape of the work
remaining without them having to count.

A review with everything tiered `blocker` is doing the tiering wrong.
A review with everything tiered `nit` is approving in disguise — just
approve.

## Validation pass

Before publishing the review, re-read it under stricter criteria. The goal
is to cut noise before the author sees it.

For each **blocker**:

- Could a competent senior engineer reasonably disagree this is a blocker?
- Would the PR cause concrete harm if merged as-is?
- If yes-to-disagreement or no-to-harm, demote to must-fix.

For each **must-fix**:

- Is this required for the change to work, or a preference?
- If preference, demote to nit.

For each **nit**:

- Is this contributing signal, or just adding length?
- If just length, cut.

For every finding regardless of tier:

- Re-read the diff at the cited line. Did you read the code correctly, or
  did you infer something the diff doesn't show?
- Could the concern be addressed in a follow-up rather than blocking this
  PR?

A review that survives this pass is the one you publish. Typical reviews
shrink 30–50% in this step. If yours doesn't, you weren't strict enough.

## Voice

Pick one voice from `voice/` at random unless the user names a specific one in
their request. The voice shapes phrasing — what to ask, what to declare, how
much to elaborate. Currently available:

- `voice/architect.md` — layered architectural reasoning, code-block alternatives.

- `voice/foreman.md` — short orders, declarative, no padding.

- `voice/pair.md` — Socratic, suggestion-blocks, ratifies progress.

State the chosen voice once at the top of the review so the author can read the
tone correctly.

These rules apply to every voice:

- When suggesting an alternative, **name it concretely** — the exact helper,
  type, or pattern. Don't gesture at "consider refactoring."

- When stating a **blocker**, be direct: "This PR cannot be accepted as is."
  No padding, no softening, regardless of the voice you picked.

- Don't explain at length unless the author pushes back or the chosen voice
  calls for it.

## Common concerns

These apply across all kinds; per-kind files sharpen them.

- **Scope discipline.** One PR, one purpose. Split incidental refactors,
  drive-by fixes, and unrelated cleanups into follow-up PRs.

- **Changelog accuracy.** User-visible changes need an entry in
  `doc/changes/{added,changed,fixed}/` per `CONTRIBUTING.md` format.
  Internal-only changes (refactors, internal perf) need none.

- **Title and description match the final diff.** When scope narrows mid-review,
  metadata must be updated.

- **Reuse existing helpers.** Check `stdune` and neighbouring modules first.
  Prefer typed paths (`Path.Local.t`, `Path.Build.t`) over strings; reuse
  existing utilities (`List.find_map`, `Section.equal`, `Path.append_local`,
  `Context.merlin`) instead of open-coding.

- **Minimal interface.** `.mli` exposes only what other modules call. No
  speculative API; no re-exports for grepability.

- **Error-path discipline.** `Error _ -> ()` or `try _ with _ -> ()` must log or
  raise — never silently swallow.

- **Doc/comment drift.** `.mli` docstrings move with the code; stale prose is a
  defect.

- **Style.** For OCaml style nits (`.mli` hygiene, pattern matching, naming,
  `Pp` printing, idioms), see `code-style.md`.
