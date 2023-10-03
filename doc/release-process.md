# Release Process

This document explains how we release dune. Its goal is to describe how things
are done in practice, not discuss how they should be done. There are two
aspects to this:

- a fairly rigid flowchart-style process for each type of release
- a softer "decision" section that explains what should inform the decisions to
  take when there is a manual call to make.

## Minor Releases (`x.y.0`)

```mermaid
gitGraph
  commit id: "feat(1)"
  commit id: "feat(2)"
  commit id: "feat(3)"
  branch "x.y-0~alpha"
  commit tag: "x.y.0~alpha1"
  checkout main
  commit id: "fix(1)"
  checkout "x.y-0~alpha"
  cherry-pick id: "fix(1)"
  commit tag: "x.y.0~alpha2"
  checkout main
  commit tag: "x.y.0"
```

```mermaid
stateDiagram-v2
    direction LR
    [*] --> Prepare
    Prepare --> Alpha
    Alpha --> Alpha
    Alpha --> Release
    Release --> PostRelease
    PostRelease --> [*]
```

- Prepare:
  - Open tracking issue with expected alpha1 date
  - List (and update) known blockers. These prevent releasing `x.y.0`
  - Add "All x.y.z changelogs merged" as blocker
  - Add "Mirage test" as blocker (manual workflow should be triggered on a
    alpha release)

- Alpha time:
  - Branch setup:
    - (for N=0) create `x.y.0~alpha` branch
    - (for N>1) merge main into alpha branch
  - Prepare alpha release:
    - cherry-pick extra commits from `main` (if any)
    - prepare changelog (ensure version is `x.y.0~alphaN`)
    - `make opam-release`
    - mark opam-repo PR as draft
  - Wait for `opam-repo-ci`
  - Triage phase:
    - consider new failures comparing from latest "known good" release
    - ignore transient errors (disk full, switch disconnected, cancelled, etc)
    - file issues about regressions, add them to known blockers
  - Release Go/No Go (go to Release, or need another alpha)
  - Mark alpha PR as closed

- Release time:
  - On main, prepare changelog (compile entries, set header with version)
  - Open a PR `prepare-x.y.0`
  - Self-merge
  - `make opam-release` from updated main
  - Triage
  - In case of regression:
    - mitigate (for example if this happens on a single OS, set `available`
      appropriately)
    - prepare point release
    - in the worst case, the release can be cancelled completely and only come
      in a point release.

- Post-release:
  - Categorize changelog entries into Added / Fixed / Changed / Removed / Deprecated
  - Open PR on `ocaml/ocaml.org` to add a file in under `data/changelog/dune`
  - Post to discuss
  - Close release milestone
  - Close tracking issue

## Point Releases / Patch Releases (`x.y.z`, `z >= 0`)

```mermaid
gitGraph
  commit tag: "x.y.0"
  commit id: "fix(1)"
  commit id: "feat(1)"
  commit id: "fix(2)"
  branch x.y
  cherry-pick id: "fix(1)"
  cherry-pick id: "fix(2)"
  commit tag: "x.y.1"
  checkout main
  commit id: "feat(2)"
  commit id: "feat(3)"
  commit id: "fix(3)"
  checkout x.y
  cherry-pick id: "fix(3)"
  commit tag: "x.y.2"
```

```mermaid
stateDiagram-v2
    direction LR
    [*] --> Prepare
    Prepare --> Backport
    Backport --> Backport
    Backport --> Release
    Release --> PostRelease
    PostRelease --> [*]
```

- Prepare:
  - Create release tracking issue
  - List fixes present in main to backport
  - Blockers for this release include:
    - all backports of listed fixes
    - changelogs of previous point releases are merged

- Backport:
  - Branch setup
    - (z=1) Create branch `x.y` from commit tagged `x.y.0`
    - (z>1) Position on branch `x.y`
  - `git cherry-pick` commits as merged in `main`
  - Open PR
    - Set `x.y` as target branch, e.g. `gh pr create -B x.y`
    - Title starts with `[x.y]`
    - List PR in blockers

- Release:
  - Position on `x.y`
  - Prepare changelog
  - Open a PR `prepare-x.y.z`
  - `make opam-release`
  - Triage

- Post-release:
  - Open PR on `ocaml/ocaml.org` to add a file in under `data/changelog/dune`
  - Post changelog on Discuss in the same thread as `x.y.0`
  - Merge changelog
  - Close release tracking issue

## Decisions

- Release cadence:
  - we aim for a minor release roughly every 4 to 6 weeks. More than 8 tends to
    make riskier releases; less than 3 would be too much overhead.
  - we do point releases only for the latest release minor version.

- Release Go/No Go after alpha:
  - the goal is to determine, once the known blockers are fixed, if we need an
    alpha(N+1) to get enough confidence about `x.y.0`
  - downside if release is Go but a bug is found: need a quick point release.
  - downside if release is No Go but not bug is found: waste of ~1 day and
    the ~50k builds.

- Determine if a change can be backported:
  - it needs to be a fix, with no version-specific behaviour
  - it needs to be merged in `main`

- Triage:
  - The thing to determine is whether a failure is a regression: considering a
    failure, would the same build plan succeed with the previous release of Dune?
    - Ultimately it's possible to run that locally, for example with `opam
      build`.
    - Comparing to the previous release is often enough; but note that some new
      packages have been added in the meantime.
  - Transient errors can be ignored or restarted; however some of them like
    "solver timed out" can not succeed. Some packages are known to fail in
    `opam-repo-ci` but there is no good way to skip them.
  - Sending metadata fixes in `opam-repository` (e.g. OCaml 5 failures) is nice
    to do but not required.
