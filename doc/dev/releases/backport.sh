#!/usr/bin/env sh

# Script name: backport.sh
# Description: This script creates backport PRs into a release candidate branch.
# Author(s): The Dune team
# Date: 2026-03-27
#
# Usage:
#
#  $ VERSION=X.Y.Z PR=<pr-number> [PR_ONLY] [DUNE_REMOTE] ./backport.sh
#
# where
#
# - X.Y.Z is the version of Dune being released
# - <pr-number> is the number of a PR merged into main that needs backporting
# - The optional PR_ONLY, if `true`, will open a PR from the current branch,
#   without preparing the backport cherry pick
# - The optional DUNE_REMOTE can be used to set name of the git remote to use as
#   the bases for created PRs. It defaults to 'git config remote.pushdefault',
#   if set, or finally to 'origin', if not.
#
# E.g.,
#
#  $ VERSION=3.22.1 PR=1234 ./backport.sh
#
# If you have had to manually resolve merge conflicts from the cherry-pick, then
# you can rerun the script from the created backport branch with PR_ONLY=true,
# open the PR into the release branch. E.g.,
#
#  $ PR_ONLY=true VERSION=X.Y.Z PR=<pr-number> ./backport.sh

set -xe
set -o pipefail

# Check for required utilities
command -v jq >/dev/null 2>&1 || (echo >&2 "script requires jq"; exit 1)
command -v gh >/dev/null 2>&1 || (echo >&2 "script requires gh"; exit 1)

# Validate and prepare input variables
[ ! -z "${PR}" ] || (echo >&2 "error: variable 'PR' is not set"; exit 1)
[ ! -z "${VERSION}" ] || (echo >&2 "error: variable 'VERSION' is not set"; exit 1)
PR_ONLY=${PR_ONLY:-false}
# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config remote.pushdefault || echo "")}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

# All variables should be set from this point on
set -u

# Computed values that are reused

rc_branch="${VERSION}-rc"
backport_branch="backport-${PR}"

# The github owner of the repo
# Required to avoid a manual prompt (see https://github.com/cli/cli/issues/5009)
github_owner=$(git remote get-url "${DUNE_REMOTE}" | sed -E 's#.*github\.com[:/]([^/]+).*#\1#')

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
    git push -u "${DUNE_REMOTE}" HEAD
    git cherry-pick --mainline 1 --signoff -x "${commit}"
fi

gh pr create \
    --repo ocaml/dune \
    --base "${rc_branch}" \
    --head "${github_owner}:${rc_branch}" \
    --title "[${VERSION}] backport #${PR}" \
    --body "Backport #${PR} onto ${rc_branch}"
