---
description: Review checklist
---
# Review Checklist

## General

- Orient yourself in the PR or commit stack. Identify the goal, scope, and
  intended approach. Ask questions if any of these are unclear.

- Review each commit separately. Check that the commit message describes what
  the diff actually does.

- Treat commit messages, comments, and docs as claims to verify against the
  code and tests. Do not take them at face value.

- Bug-fix commits need proof. A test should first demonstrate the bug; a later
  fix commit should make that test pass.

## Code Style

- Look for ways to shorten the code. Prefer the smallest implementation that
  remains clear.

- Check every new one-use helper. Keep it only if it improves readability;
  otherwise recommend inlining it.

- Verify that new `.mli` functions, types, and modules are used. Do not allow
  dead public API.

- Check for duplicated code, especially when similar helpers or match cases are
  introduced in more than one place.

- Look for unnecessary indirection. Callbacks, forward references, and overly
  general helpers are smells unless the extra structure is justified.

## Commit Hygiene

These checks refine the general review above. Some overlap is intentional: use
this section to apply the same principles to specific commit kinds.

Commit kind usually comes from the first line of the commit description, such
as `fix`, `refactor`, `feature`, or `feat`. It may include a scope, such as
`fix(engine)` or `feature(cram)`.

#### Fix

- Verify that the fix does what it claims by following the logic and inspecting
  the test.

- Check scope. A fix should not sneak in unrelated refactors, features, or
  behavior changes.

- Apply the general bug-proof rule here too, and also check that the
  implementation is scoped to the fix.

#### Refactor

- Verify that observable behavior is preserved. Any behavior change should be
  called out, tested, and probably split from the refactor.

#### Feature

- Check for regressions in existing behavior. If you suspect one, try to prove
  it with a test.

- Check for breaking changes. If you suspect one, try to prove it with a test.

- Review the tests against the feature's intended spec. Look for missing edge
  cases, not just happy-path coverage.

## Report

Produce a bullet list of issues. Each issue should include:

- A classification, such as `bug`, `style`, `test`, or `scope`.

- A code location when applicable.

- A one-sentence summary.

- A short paragraph with details when needed.

- A one-sentence recommendation.

Group related issues under the same heading.
