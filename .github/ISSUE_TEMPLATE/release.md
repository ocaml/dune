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

### Fixes

Regressions requiring fixes in `main` and backports to `X.Y.Z-rc`:

- [ ] #...
  - [ ] backported via PR #...
- [ ] #...
  - [ ] backported via PR #...
<!-- } else if MINOR OR MAJOR RELEASE { -->

## Known blockers

Regressions requiring fixes in  `main` and backports to the latest version:

- [ ] issue #... blocking the release because of ...
  - [ ] backported via PR #...
<!-- } -->

## Release

- [ ] Update changelog
<!-- If MINOR OR MAJOR RELEASE: uncomment -->
<!-- - [ ] Create release candidate branch `X.Y.Z-rc` from main -->
<!-- - [ ] Open release draft PR from branch `X.Y.Z-rc` into `main` [link to dune PR] -->
<!-- - [ ] Alpha release PRs into opam repo for triage:  -->
<!--   - [link to OPAM PR] -->
<!-- - [ ] Review revdeps diff, and diagnose any new failures -->
- [ ] opam repo publication PR [link to opam PR]

## Post-release

- [ ] Merge release branch into `main` [link to dune PR]
- [ ] Write a post about the release on Discuss [link to post]
- [ ] Store the revdeps error file in the [logs](https://github.com/ocaml/dune/wiki/Reverse-dependencies-CI-logs) as HTML
<!-- If MINOR OR MAJOR RELEASE: uncomment -->
<!-- - [ ] Increase `lang dune` number   -->
