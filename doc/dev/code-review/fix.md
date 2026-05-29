# `fix:` review concerns

When reviewing a `fix:` PR, apply these concerns in addition to those in
`general.md`.

## Concerns specific to `fix:`

- **Regression test that fails without the fix.** For behavioural fixes,
  consider landing the test pre-fix so the diff makes the effect obvious.

- **Root cause, not band-aid.** Is this fixing at the right layer?

- **Adjacent bugs of the same shape** — note as follow-up rather than
  expanding scope.

## Backports

A PR titled `[3.X.Y] backport #NNNN` is a fix (or sometimes a feature)
applied to a release branch. Extra concerns:

- **Faithful to the original.** The diff should match the merged PR being
  backported, minus changes needed to apply cleanly. Flag any drift.

- **Regression test backported too.** If the original PR added a test,
  the backport should include it. A backport without the test is a
  red flag.

- **Release-branch compatibility.** Code on a release branch may use
  older patterns. Adaptations are expected, but flag any that change
  behaviour rather than just resolve a conflict.
