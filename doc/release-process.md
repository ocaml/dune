# Release Process

```mermaid
gitGraph
  commit id: "feat(1)"
  commit id: "feat(2)"
  commit id: "feat(3)"
  branch "3.10-0~alpha"
  commit tag: "3.10.0~alpha1"
  checkout main
  commit id: "fix(1)"
  checkout "3.10-0~alpha"
  cherry-pick id: "fix(1)"
  commit tag: "3.10.0~alpha2"
  checkout main
  commit tag: "3.10.0"
  commit id: "fix(3)"
  commit id: "feat(4)"
  commit id: "fix(4)"
  commit tag: "3.10.0"
  branch 3.10
  cherry-pick id: "fix(3)"
  cherry-pick id: "fix(4)"
  commit tag: "3.10.1"
  checkout main
```

## Minor Releases (`x.y.0`)

- Prepare:
  - Open tracking issue with expected alpha1 date
  - List (and update) known blockers. These prevent releasing `x.y.0`

- Alpha time:
  - On alpha branch, prepare alpha release:
    - (for N=0) create `x.y.0~alpha` branch
    - (for N>1) merge main into alpha branch
    - cherry-pick extra commits from `main` (if any)
    - prepare changelog (ensure version is `x.y.0~alphaN`)
    - `make opam-release`
    - mark opam-repo PR as draft
  - Wait for `opam-repo-ci`
  - Triage phase:
    - consider new failures comparing from latest "known good" release
    - ignore transient errors (disk full, switch disconnected, cancelled, etc)
    - file issues about regressions, add them to known blockers
  - Go/no go for next alpha:
    - the goal is to determine, once the known blockers are fixed, if we need
      an alpha(N+1) to get enough confidence about `x.y.0`
    - downside if release is GO but a bug is found: need a quick point release.
    - downside if release is NO GO but not bug is found: waste of ~1 day and
      the ~50k builds.

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

TODO
