---
name: Release
about: Open a release tracker issue
title: "X.Y.Z release tracker"
labels: ["release"]
assignees: ''
---

## Preparation

<!-- If release is Patch release use section { -->[
- Need backport:
    - [link to PR to backport]
- Backports:
    - [link to backport PR]

<!-- } else if release is Minor release use section { -->
### Known blockers

- [ ] Something is blocking the PR because of ...

### Pre-release validation

- [ ] Validate Mirage by running https://github.com/ocaml/dune/actions/workflows/mirage.yml on the main branch
- [ ] Audit changes to ensure all significant changes have been logged
- [ ] Alpha-time! (TODO)

<!-- } -->

## Release

<!-- Replace X.Y.Z with the correct Dune version -->
- [ ] Create a new `X.Y` release branch
- [ ] Update dune changelog to `X.Y.Z` on `X.Y` branch [link to dune PR]
- [ ] Open then pull request on `opam-repository` [link to OPAM PR]
- [ ] Triage (ensure it does not break anything)
- [ ] Update nix-overlays with the new version [link to nix-overlays PRs]

## Post-release

- [ ] Merge dune changelog in `main` [link to dune PR]
- [ ] Update ocaml.org changelog [link to ocaml.org PR]
- [ ] Write a post about the release on Discuss [link to post]
- [ ] Store the revdeps error file in the [logs](https://github.com/ocaml/dune/wiki/Reverse-dependencies-CI-logs)
- [ ] Create a next release milestone
<!-- If minor release uncomment this -->
<!-- - [ ] Increase `lang dune` number   -->

## Last stage

- [ ] Close tracking issue
