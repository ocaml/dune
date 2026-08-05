# Documentation and scripts for managing releases

See [./process.md](./process.md) for the end-to-end release process. The scripts
below automate its mechanical steps, listed in the order they are typically used:

- [./release-init.sh](./release-init.sh) start a release: create the tracking
  issue, cut the release candidate branch, and open the draft release PR.
- [./backport.sh](./backport.sh) backport the commits in a PR merged or squashed
  (but NOT rebased) into `main` into a release candidate branch.
- [./release-cut.sh](./release-cut.sh) cut a (pre)release from a release
  candidate branch: prepare the changelog, commit and push it, and run
  `dune-release` through its publication flow.
