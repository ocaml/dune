#!/usr/bin/env bash

# Script name: release-init.sh
# Description: This script initiates the release process for a release of dune
# Author(s): The Dune team
# Date: 2026-04-09
#
# Usage:
#
#  $ INCREMENT=(patch|minor|major) [DUNE_REMOTE] [DUNE_REPO] [ISSUE_URL] ./release-init.sh
#
# where
#
# - INCREMENT is the version increment to start releasing
# - The optional DUNE_REMOTE can be used to set name of the git remote to use as
#   the bases for created PRs. It defaults to 'git config remote.pushdefault' if
#   set or finally to 'origin' if not.
# - The optional DUNE_REPO can be set to your personal fork, to open issues
#   and PRs there instead of on ocaml/dune. Intended for testing.
# - The optional ISSUE_URL will use the provided release tracking issue URL
#   instead of creating a new issue for it.
#
# E.g.,
#
#  $ INCREMENT=patch ./release-init.sh
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
[[ -n "${INCREMENT:-}" ]] || err "variable INCREMENT is not set"
DUNE_REPO=${DUNE_REPO:-"ocaml/dune"}
# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config --get remote.pushdefault || true)}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

# Check for required utilities
command -v gh >/dev/null 2>&1 || err "script requires gh"
command -v git >/dev/null 2>&1 || err "script requires git"

# All variables should be set from this point on
set -u

# The github owner of the repo is required to avoid a manual prompt when running
# gh pr create in the final steps (see https://github.com/cli/cli/issues/5009)
REMOTE_URL=$(git remote get-url "${DUNE_REMOTE}") \
    || err "git remote '${DUNE_REMOTE}' does not exist"
GITHUB_OWNER=$(printf '%s\n' "${REMOTE_URL}" | sed -nE 's#.*github\.com[:/]([^/]+)/.*#\1#p')
[[ -n "${GITHUB_OWNER}" ]] || \
    err "could not determine GitHub owner from remote '${DUNE_REMOTE}'"

# For testing, you can set this to your fork
DUNE_GIT_URL="https://github.com/${DUNE_REPO}.git"

POST_RELEASE_STEPS="## Post-release

- [ ] Merge release branch into \`main\`
- [ ] Write a post about the release on Discuss: [link]
- [ ] Open PR announcing release on ocaml.org: [link]
- [ ] Store the revdeps error file in the [logs](https://github.com/ocaml/dune/wiki/Reverse-dependencies-CI-logs) as HTML
"

MINOR_OR_MAJOR_BODY="## Known blockers

Issues blocking the release

- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...
- [ ] issue #... blocking the release because of ...
  - [ ] resolved by PR #...

## Preparation

To begin once all initial blockers are resolved.

- [ ] Create the release candidate branch and ensure all CI jobs are passing
- [ ] Work thru the [pre-release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#pre-release-phase).
  Alpha release PRs into opam repo:
  - [link to OPAM PR 1]
  - [link to OPAM PR 2]

## Release

- [ ] Work thru the [release phase](https://github.com/ocaml/dune/blob/main/doc/dev/releases/process.md#release-phase).
- [ ] opam repo publication PR [link to opam PR]

${POST_RELEASE_STEPS}
- [ ] Increase the version of Dune to the new latest minor version
  - [ ] in the [CI workflow](https://github.com/ocaml/dune/tree/main/.github/workflows/workflow.yml.in)
  - [ ] in [dune-project](https://github.com/ocaml/dune/blob/main/dune-project#L1)
  - [ ] in the [dune-rpc](https://github.com/ocaml/dune/blob/main/otherlibs/dune-rpc/types.ml#L30)
"

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

function last_version_of_pattern () {
    local pattern
    pattern="$1"

    local last_version
    last_version=$(git -c versionsort.suffix="_alpha" tag --list "${pattern}" --sort=-version:refname | head -n 1)
    [[ -n "${last_version}" ]] || err "could not find a previous release tag matching pattern ${pattern}"

    echo "${last_version}"
}

function patch_release () {
    local last_version
    last_version=$(last_version_of_pattern '*.*.*')
    next_version=$(echo "$last_version" | awk -F. -v OFS=. '{print $1, $2, $3+1 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="## Fixes

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
"
    git switch -c "${rc_branch}" "${last_version}"
}

function minor_release () {
    next_version=$(last_version_of_pattern '*.*.0' | awk -F. -v OFS=. '{print $1, $2+1, 0 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="${MINOR_OR_MAJOR_BODY}"
    git fetch "${DUNE_GIT_URL}" main
    git switch -c "${rc_branch}" FETCH_HEAD
}

function major_release () {
    next_version=$(last_version_of_pattern '*.0.0' | awk -F. -v OFS=. '{print $1+1, 0, 0 }')
    rc_branch="${next_version}-rc"

    confirm

    issue_body="${MINOR_OR_MAJOR_BODY}"
    git fetch "${DUNE_GIT_URL}" main
    git switch -c "${rc_branch}" FETCH_HEAD
}

# For version sorting, see https://git-scm.com/docs/git-tag#Documentation/git-tag.txt---sortkey
git fetch --tags "${DUNE_REMOTE}"

case "${INCREMENT}" in
    "patch") patch_release ;;
    "minor") minor_release ;;
    "major") major_release ;;
    *) err "INCREMENT must be 'patch' 'minor' or 'major'" ;;
esac

if [[ -n "${ISSUE_URL:-}" ]]
then
    issue_url="${ISSUE_URL}"
else
    issue_url=$(gh issue create \
        --repo "${DUNE_REPO}" \
        --assignee "@me" \
        --label "release" \
        --title "${next_version} release tracker" \
        --body "${issue_body}")
fi

# Empty commit necessary to start a PR that initially has no diff
git commit --allow-empty --signoff -m "[${next_version}] initiate release"

git push -u "${DUNE_REMOTE}" HEAD
gh pr create \
    --draft \
    --repo "${DUNE_REPO}" \
    --head "${GITHUB_OWNER}:${rc_branch}" \
    --base "main" \
    --title "[${next_version}] release" \
    --body "Release branch for ${issue_url}"
