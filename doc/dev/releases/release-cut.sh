#!/usr/bin/env bash

# Script name: release-cut.sh
# Description: This script cuts a release from a release candidate branch of dune
# Author(s): The Dune team
# Date: 2026-05-01
#
# Usage:
#
#  $ RELEASE_KIND=(release|prerelease) [DRY_RUN=true] [DUNE_REMOTE=<remote>] ./release-cut.sh
#
# where
#
# - RELEASE_KIND indicates whether the release should be a prerelease (an alpha)
#   or a full release.
# - The optional DRY_RUN, when set to 'true', prints the mutating commands
#   instead of running them, so the release flow can be previewed safely.
# - The optional DUNE_REMOTE can be used to set the name of the git remote to
#   fetch tags from and push the release branch to. It defaults to 'git config
#   remote.pushdefault' if set or finally to 'origin' if not.
# - NB. To stage a release against forks, dune-release reads the following from
#   the environment; they pass through 'make opam-release' unchanged:
#   - DUNE_RELEASE_DEV_REPO    the dune fork the release/tag/tarball goes to
#                              (a plain git URL, e.g. https://..., not git+https)
#   - DUNE_RELEASE_OPAM_REPO   the opam-repository the package PR is opened into
#                              (owner/repo form)
#   - DUNE_RELEASE_REMOTE_REPO your fork of opam-repository to push the PR from
#                              (a git URL, e.g. git@github.com:you/opam-repository.git)
#   - DUNE_RELEASE_LOCAL_REPO  path to a local clone of that opam-repository fork
#   Set DUNE_REMOTE to the matching dune fork remote so the release branch and
#   changelog commit are pushed there too.
#
# E.g.,
#
#  $ RELEASE_KIND=prerelease ./release-cut.sh
#
# and, to stage the whole flow against personal forks,
#
#  $ RELEASE_KIND=prerelease \
#      DUNE_REMOTE=my-fork \
#      DUNE_RELEASE_DEV_REPO=https://github.com/me/dune.git \
#      DUNE_RELEASE_OPAM_REPO=me/opam-repository \
#      ./release-cut.sh
#
# The script will prepare the change log, commit and push updates, and run dune-release
# thru its entire release and publication flow.

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

# Prompt for confirmation before running the irreversible release steps
function confirm () {
    local release_version="$1"
    read \
        -p "About to cut ${RELEASE_KIND} ${release_version} on branch '${branch}', push to '${DUNE_REMOTE}', and publish via dune-release. Continue? (y/Y) " \
        -n 1 -r
    echo # Print a newline since -n 1 suppresses the newline after input
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "Proceeding..."
    else
        echo "Aborted."
        exit 1
    fi
}

# Validate and prepare input variables
[ ! -z "${RELEASE_KIND}" ] || err "variable RELEASE_KIND is not set"
# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config remote.pushdefault || echo "")}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(realpath "${SCRIPT_DIR}/../../..")

# Check for required utilities
command -v git >/dev/null 2>&1 || err "script requires git"
command -v dune-release >/dev/null 2>&1 || err "script requires dune-release"

# All variables should be set from this point on
set -u

# Parser a version from a git tag and produces an incremented prerelease version
function increment_prerelease () {
    local v="$1"
    if [[ "$v" =~ ^(.*)_alpha([0-9]+)$ ]]; then
        base="${BASH_REMATCH[1]}"
        n="${BASH_REMATCH[2]}"
        res="${base}~alpha$((n + 1))"
    else
        res="${v}~alpha0"
    fi
    echo "$res"
}
# increment_prerelease "1.1.2"         # 1.1.2~alpha0
# increment_prerelease "1.1.2_alpha0"  # 1.1.2~alpha1
# increment_prerelease "1.1.2_alpha5"  # 1.1.2~alpha6

# extract just the version from a string with a semantic version as prefix
function extract_version_prefix () {
    local input="$1"
    local err_msg=${2:-"extract_version_prefix: invalid version string: $1"}
    if [[ "${input}" =~ ^([0-9]+\.[0-9]+\.[0-9]+)(-rc|[~_]alpha[0-9]+)?$ ]]; then
        echo "${BASH_REMATCH[1]}"
    else
        err "${err_msg}"
    fi
}
# extract_version_prefix "1.1.1-rc"                     # 1.1.1
# extract_version_prefix "1.2.1~alpha10"                # 1.2.1
# extract_version_prefix "1.2.1-alpha10"                # default error message
# extract_version_prefix "1.2.1-alpha10" "custom error" # custom error message

git fetch --tags "${DUNE_REMOTE}"
branch=$(git branch --show-current)
version=$(extract_version_prefix "${branch}" \
    "must be run from a release candidate branch matching 'x.y.z-rc' but run from ${branch}")
# versionsort.suffix="_alpha" ensures _alpha suffixes are sorted as preceding the actual version release
last_version=$(git -c versionsort.suffix="_alpha" tag --list "${version}" "${version}_alpha*" --sort=-version:refname | head -n 1 )
if [[ -n "${last_version}" ]]; then
    last_version_prefix=$(extract_version_prefix "${last_version}")
else
    last_version_prefix=""   # no prior tags for this version
fi

# Drop the changelog section for the in-progress version, if the changelog
# already opens with one (e.g. from a previous alpha), so the regenerated
# section replaces it instead of stacking a duplicate. The change fragments
# remain the source of truth; this only removes the stale rendering.
function strip_in_progress_section () {
    local changes="${ROOT_DIR}/CHANGES.md" tmp
    tmp=$(mktemp "${changes}.XXXXXX") || err "could not create a temporary file"
    if awk -v vp="${version}" '
        # Return the X.Y.Z of a version header like "3.24.0 (2026-06-21)",
        # or "" if the line is not a version header.
        # The second parameter, `v`, is a local variable, not an input argument.
        function header_version(line,   v) {
            if (line !~ /^[0-9]+\.[0-9]+\.[0-9]+[^ ]* \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\)$/) {
                return ""
            } else {
                v = line
                sub(/ .*/, "", v)         # drop " (date)"  -> "3.24.0" or "3.24.0~alpha1"
                sub(/[~_-].*/, "", v)     # drop the suffix -> "3.24.0"
                return v
            }
        }
        {
            v = header_version($0)
            if (v != "") {
                # A version header starts a section. Skip exactly one section:
                # the first (top-most), and only if it is the in-progress version.
                section++
                skip = (section == 1 && v == vp)
            }
            if (!skip) print
        }
    ' "${changes}" > "${tmp}"; then
        mv "${tmp}" "${changes}"
    else
        rm -f "${tmp}"
        err "failed to update ${changes}"
    fi
}

# Regenerate the in-progress changelog section from the change fragments in
# doc/changes. Those fragments are the single source of truth: they are consumed
# (deleted) only for a full release, so successive prereleases regenerate the
# section in place from the accumulating fragments. Any existing section for the
# in-progress version is stripped first so the regenerated one replaces it.
function update_changelog () {
    local release_version="$1"
    local keep_fragments=true
    if [[ "${RELEASE_KIND}" == "release" ]]; then
        keep_fragments=false
    fi
    run_cmd strip_in_progress_section
    run_cmd env "KEEP_FRAGMENTS=${keep_fragments}" \
        "${ROOT_DIR}/doc/changes/scripts/build_changelog.sh" "${release_version}"
}

function pre_release_version () {
    if [[ "${version}" == "${last_version}" ]]
    then
        err "cannot cut a pre-release on branch ${branch} because the last version is ${version}"
    elif [[ "${version}" == "${last_version_prefix}" ]]
    then
        increment_prerelease "${last_version}"
    else
        increment_prerelease "${version}"
    fi
}

function release () {
    local release_version="$1"
    if [[ "${DRY_RUN:-false}" != "true" ]]; then
        confirm "${release_version}"
    fi
    run_cmd git pull --ff-only "${DUNE_REMOTE}" "${branch}"
    update_changelog "${release_version}"
    run_cmd git add "${ROOT_DIR}/doc/changes" "${ROOT_DIR}/CHANGES.md"
    run_cmd git commit -s -m "[${release_version}] prepare release"
    run_cmd git push -u "${DUNE_REMOTE}" "${branch}"
    run_cmd make RELEASE_KIND="$RELEASE_KIND" DUNE_RELEASE_YES=true opam-release
}

case "${RELEASE_KIND}" in
    "prerelease") release_version=$(pre_release_version) ;;
    "release") release_version="${version}" ;;
    *) err "RELEASE_KIND must be 'prerelease' or 'release'" ;;
esac

release "$release_version"
