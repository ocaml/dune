#!/usr/bin/env sh

# Script name: backport.sh
# Description: This script creates backport PRs into a release candidate branch.
# Author(s): The Dune team
# Date: 2026-03-27
#
# Usage:
#
#  $ VERSION=X.Y.Z PR=<pr-number> ./backport.sh
#
# where
#
# - X.Y.Z is the version of Dune being released
# - <pr-number> is the number of a PR merged into main that needs backporting
#
# If you have had to manually resolve merge conflicts from the cherry-pick, then
# you can rerun the script from the created backport branch with PR_ONLY=true, open
# the PR into the release branch. E.g.,
#
#  $ PR_ONLY=true VERSION=X.Y.Z PR=<pr-number> ./backport.sh
#

set -xe
set -o pipefail

# Check for required utilities
command -v jq >/dev/null 2>&1 || (echo >&2 "script requires jq"; exit 1)
command -v gh >/dev/null 2>&1 || (echo >&2 "script requires gh"; exit 1)

# Validate and prepare input variables
[ ! -z "${PR}" ] || (echo >&2 "error: variable 'PR' is not set"; exit 1)
[ ! -z "${VERSION}" ] || (echo >&2 "error: variable 'VERSION' is not set"; exit 1)
PR_ONLY=${PR_ONLY:-false}

# All variables should be set from this point on
set -u

# Computed values that are reused

rc_branch="${VERSION}-rc"
backport_branch="backport-${PR}"

configured_remote=$(git config remote.pushdefault || echo "")
remote=${configured_remote:-"origin"}

if [ "${PR_ONLY}" == false ]
then
    current_branch=$(git rev-parse --abbrev-ref HEAD)
    [ "${current_branch}" == "${rc_branch}" ] || git checkout "${rc_branch}" || \
        ( echo >&2 "error: expected a release candidate branch ${VERSION}-rc to exist"; exit 1 )

    # The target PR's merge commit
    commit=$(gh pr view "${PR}" --json mergeCommit --jq '.mergeCommit.oid')
    if [ -z "${commit}" ]
    then
        ( echo >&2 "error: PR ${PR} has no merge commit. Has it been merged?"; exit 1 )
    fi

    git checkout -b "${backport_branch}"
    git push -u "${remote}" HEAD
    git cherry-pick --mainline 1 --signoff -x "${commit}"
fi

gh pr create \
    --repo ocaml/dune \
    --base "${rc_branch}" \
    --title "[${VERSION}] backport #${PR}" \
    --body "Backport #${PR} onto ${rc_branch}"
