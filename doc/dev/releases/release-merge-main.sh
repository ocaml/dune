#!/usr/bin/env bash

# Script name: release-merge-main.sh
# Description: This script merges main into a release candidate branch after
# the release has been cut, so that the branch can be merged back into main
# Author(s): The Dune team
# Date: 2026-11-13
#
# Usage:
#
#  $ [DRY_RUN=true] [DUNE_REMOTE=<remote>] ./release-merge-main.sh
#
# where
#
# - The optional DRY_RUN, when set to 'true', runs all the checks but prints
#   the mutating commands instead of running them.
# - The optional DUNE_REMOTE can be used to set the name of the git remote to
#   fetch main and the release tag from. It defaults to 'git config
#   remote.pushdefault' if set or finally to 'origin' if not.
#
# The script must be run from the release candidate branch 'x.y.z-rc', after
# the release 'x.y.z' has been tagged.
#
# The resulting merge commit has the same content as main, except that
#
# - CHANGES.md is taken from the release branch, and
# - the change fragments consumed by the release are removed.
#
# Nothing else is taken from the release branch, so no conflicts can arise.
# This is sound because of two rules of the release process:
#
# - Every change on a release branch is a backport of a change merged in main.
# - CHANGES.md is only ever changed on a release branch, by build_changelog.sh.
#
# The script does not push. Push the branch and merge it into main with a merge
# commit, so that the release tag is reachable from main.

set -e
set -o pipefail

function err () {
    local msg=$1
    echo >&2 "error: ${msg}"
    exit 1
}

# run command if DRY_RUN is false or not set, else just print the command
function run_cmd () {
    if [[ "${DRY_RUN:-false}" == "true" ]]; then
        echo "DRY RUN: $*"
    else
        "$@"
    fi
}

# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config remote.pushdefault || echo "")}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(realpath "${SCRIPT_DIR}/../../..")

# Check for required utilities
command -v git >/dev/null 2>&1 || err "script requires git"

# All variables should be set from this point on
set -u

# All paths given to git below are relative to the root of the repository
cd "${ROOT_DIR}"

changelog="CHANGES.md"
# The directories holding the change fragments consumed by build_changelog.sh
fragment_dirs=(doc/changes/added doc/changes/changed doc/changes/fixed)

git fetch --tags "${DUNE_REMOTE}" main
main="${DUNE_REMOTE}/main"

branch=$(git branch --show-current)
if [[ "${branch}" =~ ^([0-9]+\.[0-9]+\.[0-9]+)-rc$ ]]; then
    version="${BASH_REMATCH[1]}"
else
    err "must be run from a release candidate branch matching 'x.y.z-rc' but run from '${branch}'"
fi

## Check that the release branch is in the expected state

git diff --quiet HEAD \
    || err "the working tree has uncommitted changes"

tag_commit=$(git rev-parse --verify --quiet "refs/tags/${version}^{commit}") \
    || err "the release tag '${version}' does not exist: cut the release first"

[[ "$(git rev-parse HEAD)" == "${tag_commit}" ]] \
    || err "'${branch}' is not at the release tag '${version}'"

if git merge-base --is-ancestor "${main}" HEAD; then
    err "'${main}' is already merged into '${branch}'"
fi

base=$(git merge-base "${main}" HEAD)

git diff --quiet "${base}" "${main}" -- "${changelog}" \
    || err "${changelog} was changed on main since '${branch}' branched off: it must only be changed on a release branch"

## Collect the release section of the changelog and the consumed fragments

# The section of the changelog for this release, used to tell whether the text
# of a change fragment has been released.
section=$(mktemp) || err "could not create a temporary file"
trap 'rm -f "${section}"' EXIT

git show "HEAD:${changelog}" | awk '
    /^[0-9]+\.[0-9]+\.[0-9]+[^ ]* \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\)$/ {
        section++
    }
    section == 1 { print }
' > "${section}"

[[ "$(head -n 1 "${section}")" == "${version} ("* ]] \
    || err "${changelog} does not open with a section for ${version}"

# The fragments corresponding to released changes are are all deleted on the
# release branch. This includes the fragments that were backported from main
# after the release branch was created, which a regular merge would leave on
# main.
mapfile -t consumed < <(
    git log --diff-filter=D --name-only --format= "${base}..HEAD" -- "${fragment_dirs[@]}" \
        | grep '\.md$' \
        | sort -u
)
declare -A is_consumed=()
for fragment in "${consumed[@]}"; do
    is_consumed["${fragment}"]=true
done

## Check that the fragments on main agree with the released changelog

# Print the lines of a fragment on main that are not in the release section
function unreleased_lines () {
    local fragment="$1"
    git show "${main}:${fragment}" | grep -v '^$' | grep -vxFf "${section}" || true
}

errors=0

function report_error () {
    echo >&2 "error: $1"
    errors=$((errors + 1))
}

# A consumed fragment must not carry text on main that was not released,
# because it is about to be removed.
for fragment in "${consumed[@]}"; do
    if git cat-file -e "${main}:${fragment}" 2>/dev/null \
            && [[ -n "$(unreleased_lines "${fragment}")" ]]; then
        report_error "${fragment}: differs on main from the text released in ${version}"
    fi
done

# A fragment that stays on main must not repeat text that was released,
# because it would be published again in the next release.
while IFS= read -r fragment; do
    if [[ -z "${is_consumed[${fragment}]:-}" ]] \
            && [[ -z "$(unreleased_lines "${fragment}")" ]]; then
        report_error "${fragment}: already released in ${version} but not consumed on '${branch}'"
    fi
done < <(git ls-tree -r --name-only "${main}" -- "${fragment_dirs[@]}" | grep '\.md$')

if ((errors > 0)); then
    err "the change fragments on main do not agree with ${changelog}: fix them on main and retry"
fi

## Merge

# Record the merge without merging any content
run_cmd git merge -s ours --no-commit "${main}"

# Take everything from main, except for the changelog
run_cmd git restore --source="${main}" --staged --worktree -- . ":(exclude)${changelog}"

# Remove the fragments consumed by the release
if ((${#consumed[@]} > 0)); then
    run_cmd git rm -q -f --ignore-unmatch -- "${consumed[@]}"
fi

# Check that the result only differs from main in the changelog and in the
# removal of change fragments
if [[ "${DRY_RUN:-false}" != "true" ]]; then
    unexpected=$(
        git diff --cached --name-only "${main}" -- . ":(exclude)${changelog}" ":(exclude)doc/changes"
        git diff --cached --name-only --diff-filter=d "${main}" -- doc/changes
    )
    if [[ -n "${unexpected}" ]]; then
        git merge --abort
        err "the merge changes files it should not, so it was aborted:
${unexpected}"
    fi
fi

run_cmd git commit -q -s -m "[${version}] merge main after release"

echo "Merged '${main}' into '${branch}'."
echo "Push '${branch}' and merge it into main with a merge commit."
