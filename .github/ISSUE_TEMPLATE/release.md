---
name: Release
about: Open a release tracker issue
title: "X.Y.Z release tracker"
labels: ["release"]
assignees: ''
---

<!-- TODO: Replace X.Y.Z with the correct Dune version -->

<!-- if PATCH RELEASE { -->
## Preparation

- [ ] Create release candidate branch `X.Y.Z-rc` from last point tag `X.Y.Z-1`
- [ ] Open release draft PR from branch `X.Y.Z-rc` into `main` [link to dune PR]

## Fixes

Regressions requiring fixes in `main` and backports to `X.Y.Z-rc`:

- [ ] #...
  - [ ] backported via PR #...
- [ ] #...
  - [ ] backported via PR #...

For each PR needing a backport

- Run `VERSION=X.Y.Z PR=<pr-number> ./doc/dev/releases/backport.sh`
- Wait for the PR into the rc branch to pass CI.
- Merge the PR, and record it above, completing the checklist item.

## Release

- [ ] Work thru the [point release process][point-release].
- [ ] opam repo publication PR [link to opam PR]

[point-release]: https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#point-releases--patch-releases-xyz-z--0

<!-- } else if MINOR OR MAJOR RELEASE { -->

## Known blockers

Issues blocking the release

- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...
- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...

## Preparation

To begin once all initial blockers are resolved.

- [ ] Run pre-release CI jobs on `main` branch
    - [ ] [mirage](https://github.com/ocaml/dune/actions/workflows/mirage.yml)
    - [ ] [packaging](https://github.com/ocaml/dune/actions/workflows/isolated-package-build.yml)
- [ ] Work thru the [pre-release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#pre-release-phase).
  Alpha release PRs into opam repo:
  - [link to OPAM PR 1]
  - [link to OPAM PR 2]
- [ ] Draft PR from the release branch `X.Y.Z-rc` into `main`: [link to dune PR]

## Release

- [ ] Work thru the [release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#release-phase).
- [ ] opam repo publication PR [link to opam PR]
<!-- } -->

## Post-release

- [ ] Merge release branch into `main` [link to dune PR]
- [ ] Write a post about the release on Discuss [link to post]
- [ ] Store the revdeps error file in the [logs](https://github.com/ocaml/dune/wiki/Reverse-dependencies-CI-logs) as HTML
<!-- If MINOR OR MAJOR RELEASE: uncomment -->
<!-- - [ ] Increase the version of Dune to the new latest minor version -->
<!--   - [ ] in the [CI workflow](https://github.com/ocaml/dune/tree/main/ci/workflow.yml.in) -->
<!--   - [ ] in [dune-project](https://github.com/ocaml/dune/blob/main/dune-project#L1) -->
<!--   - [ ] in the [dune-rpc](https://github.com/ocaml/dune/blob/main/otherlibs/dune-rpc/types.ml#L30). -->
