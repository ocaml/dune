#!/usr/bin/env sh

# Script name: release-init.sh
# Description: This script initiates the release process for a release of dune
# Author(s): The Dune team
# Date: 2026-04-09
#
# Usage:
#
#  $ INCREMENT=(patch|minor|major) [DUNE_REMOTE] [DUNE_REPO] ./release-init.sh
#
# where
#
# - INCREMENT is the version increment to start releasing
# - The optional DUNE_REMOTE can be used to set name of the git remote to use as
#   the bases for created PRs. It defaults to 'git config remote.pushdefault' if
#   set or finally to 'origin' if not.
# - The optional DUNE_REPO can be set to your personal fork, to open issues
#   and PRs there instead of on ocaml/dune. Intended for testing.
#
# E.g.,
#
#  $ VERSION=patch ./release-init.sh
#
# The script will create the appropriate tracking issue, and start a release
# candidate branch from the appropriate branch point.

set -e
set -o pipefail

function err () {
    local msg=$1
    echo >&2 "error: ${msg}"
    exit 1
}

# Validate and prepare input variables
[ ! -z "${INCREMENT}" ] || err "variable INCREMENT is not set"
DUNE_REPO=${DUNE_REPO:-"ocaml/dune"}
# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config remote.pushdefault || echo "")}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

# Check for required utilities
command -v gh >/dev/null 2>&1 || err "script requires gh"
command -v git >/dev/null 2>&1 || err "script requires git"

# All variables should be set from this point on
set -u

# For testing, you can set this to your fork
DUNE_GIT_URL="https://github.com/${DUNE_REPO}.git"

POST_RELEASE_STEPS="""## Post-release

- [ ] Merge release branch into \`main\` [link to dune PR]
- [ ] Write a post about the release on Discuss [link to post]
- [ ] Store the revdeps error file in the [logs](https://github.com/ocaml/dune/wiki/Reverse-dependencies-CI-logs) as HTML
"""

MINOR_OR_MAJOR_BODY="""## Known blockers

Issues blocking the release

- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...
- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...

## Preparation

To begin once all initial blockers are resolved.

- [ ] Run pre-release CI jobs on \`main\` branch
    - [ ] [mirage](https://github.com/ocaml/dune/actions/workflows/mirage.yml)
    - [ ] [packaging](https://github.com/ocaml/dune/actions/workflows/isolated-package-build.yml)
- [ ] Work thru the [pre-release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#pre-release-phase).
  Alpha release PRs into opam repo:
  - [link to OPAM PR 1]
  - [link to OPAM PR 2]

## Release

- [ ] Work thru the [release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#release-phase).
- [ ] opam repo publication PR [link to opam PR]

${POST_RELEASE_STEPS}
- [ ] Increase the version of Dune to the new latest minor version
  - [ ] in the [CI workflow](https://github.com/ocaml/dune/tree/main/ci/workflow.yml.in)
  - [ ] in [dune-project](https://github.com/ocaml/dune/blob/main/dune-project#L1)
  - [ ] in the [dune-rpc](https://github.com/ocaml/dune/blob/main/otherlibs/dune-rpc/types.ml#L30)
"""

function confirm () {
    read \
        -p "Are you sure you want to initiate the ${INCREMENT} release process for version ${next_version} on the ${DUNE_REPO} repo? (y/Y) " \
        -n 1 -r
    echo # Print a newline since -n 1 suppresses the newline after input

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "Proceeding..."
    else
        echo "Aborted."
        exit 1
    fi
}

# Global return variables for the following *_release functions
next_version=
issue_body=
rc_branch=

function patch_release () {
    local last_version=$(git tag --list '*.*.*' --sort=-version:refname | head -n 1 )
    next_version=$(echo "${last_version}" | awk -F. -v OFS=. '{print $1, $2, $3+1 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="""## Fixes

Regressions requiring fixes in \`main\` and backports to \`${next_version}-rc\`:

- [ ] #...
  - [ ] backported via PR #...
- [ ] #...
  - [ ] backported via PR #...

For each PR needing a backport

- Run \`VERSION=${next_version} PR=<pr-number> ./doc/dev/releases/backport.sh\`
- Wait for the PR into the rc branch to pass CI.
- Merge the PR, and record it above, completing the checklist item.

## Release

- [ ] Work thru the [point release process][point-release].
- [ ] opam repo publication PR [link to opam PR]

[point-release]: https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#point-releases--patch-releases-xyz-z--0

${POST_RELEASE_STEPS}
"""
    git switch -c "${rc_branch}" "${last_version}"
}

function minor_release () {
    next_version=$(git tag --list '*.*.0' --sort=-version:refname | \
        head -n 1 | \
        awk -F. -v OFS=. '{print $1, $2+1, 0 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="${MINOR_OR_MAJOR_BODY}"
    git fetch "${DUNE_GIT_URL}" main
    git switch -c "${rc_branch}" FETCH_HEAD
}

function major_release () {
    next_version=$(git tag --list '*.0.0' --sort=-version:refname | \
        head -n 1 | \
        awk -F. -v OFS=. '{print $1+1, 0, 0 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="${MINOR_OR_MAJOR_BODY}"
    git fetch "${DUNE_GIT_URL}" main
    git switch -c "${rc_branch}" FETCH_HEAD
}

# For version sorting, see https://git-scm.com/docs/git-tag#Documentation/git-tag.txt---sortkey
git fetch --tags
case "${INCREMENT}" in
    "patch") patch_release ;;
    "minor") minor_release ;;
    "major") major_release ;;
    *) err "INCREMENT must be 'patch' 'minor' or 'major'" ;;
esac

issue_url=$(gh issue create \
    --repo "${DUNE_REPO}" \
    --assignee "@me" \
    --label "release" \
    --title "${next_version} release tracker" \
    --body "${issue_body}")

# Empty commit necessary to start a PR that initially has no diff
git commit --allow-empty --signoff -m "[${next_version}] initiate release"

# The github owner of the repo
# Required to avoid a manual prompt (see https://github.com/cli/cli/issues/5009)
github_owner=$(git remote get-url "${DUNE_REMOTE}" | sed -E 's#.*github\.com[:/]([^/]+).*#\1#')

git push -u "${DUNE_REMOTE}" HEAD
gh pr create \
    --draft \
    --repo "${DUNE_REPO}" \
    --head "${github_owner}:${rc_branch}" \
    --base "main" \
    --title "[${next_version}] release" \
    --body "Release branch for ${issue_url}"
