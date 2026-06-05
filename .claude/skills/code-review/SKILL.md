---
name: code-review
description: Reviews code changes for bugs, implementation quality, and test coverage. Use when reviewing commits, pull requests, or diffs.
---

# Code Review

The substantive review guidance lives in `doc/dev/code-review/`:

- `general.md` — classification, the three questions, voice, common concerns.
  **Read this first.**

- `feat.md`, `fix.md`, `refactor.md`, `test.md`, `docs.md` — kind-specific
  concerns. Read the one matching the PR's classification.

- `code-style.md` — OCaml style guide. Read when style nits surface.

- `voice/` — review voices. Default to `voice/foreman.md` unless the user
  names a different voice in their request.

This file (the skill) covers the agent-specific tooling for executing those
steps.

## Prerequisites

Detect available tools before issuing commands:

- **VCS**: prefer `jj` if installed (check for `.jj/`), otherwise `git`.

- **GitHub CLI**: `gh` requires `gh auth status` to succeed; if not, ask the
  user to paste PR/issue content.

## Identify what to review

Use `jj show <rev>` (or `git show <rev>`) to examine the change. If the target
has ancestor commits not yet on main, ask whether to review the entire branch or
just the specified change.

## Fetch the stated intent

See `doc/dev/code-review/general.md` for what to extract. Useful commands:

- `gh pr view <number>` — PR description

- `gh issue view <number>` — linked issue

- `jj show <rev>` (or `git show <rev>`) — commit message body

## Two-pass workflow via subagents

Reviews benefit from a separate validator. Use a two-pass workflow:

1. **Draft pass.** Spawn one subagent (or one per voice, in parallel) to
   write the draft review against the guidance in `doc/dev/code-review/`.
   The drafter follows the voice file, the kind file, and the three
   questions.

2. **Validation pass.** Spawn a **separate, fresh-context subagent** to
   read the diff and the draft, then tighten the draft per the
   "Validation pass" criteria in `general.md`. The validator must not
   have written the draft — fresh context is what makes it useful.

For high-stakes reviews (public API, security, large PRs), always run
both passes. For trivial PRs (typos, single-line fixes), one pass is fine
and the validation criteria fold into the drafter's own re-read.

Subagent constraints:

- The drafter has full read access. The validator should be read-only
  (no Edit/Write/Bash mutations) — its job is to judge the draft, not
  rewrite the code.

- Prefer passing the diff and any pre-state files in via the prompt. Only
  stage to a path on disk if the content is too large to fit comfortably in
  a prompt; in that case use an existing scratch location rather than
  creating a new directory in the repo (per `AGENTS.md`: avoid creating
  files unless necessary).

- Sandbox often blocks file writes from subagents. Have each subagent
  return the review inline in its final response between delimiters
  (e.g. `==BEGIN REVIEW==` / `==END REVIEW==`) so the parent can extract
  and save it.

### Validator subagent prompt template

```
You are validating a draft code review. You did not write it.

Read:
- The diff at <path>
- The draft review at <path>
- doc/dev/code-review/general.md (especially "Tier every issue" and
  "Validation pass")

For each finding in the draft, apply the validation criteria. Demote
over-tiered findings, cut findings that don't survive the senior-engineer
disagreement test, and re-read the cited diff lines to catch misreads.

Return the revised review in the same structure, plus a
"## Changes from draft" appendix listing what you cut or demoted and why.

Be skeptical by default. Aim for a 30-50% reduction in finding count if
the draft hasn't already self-validated.
```
