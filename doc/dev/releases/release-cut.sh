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
# - The optional DUNE_REPO is the repository whose CI results gate the release,
#   in owner/repo form. It defaults to 'ocaml/dune'. Set it to your fork when
#   staging a release, since it is resolved independently of DUNE_REMOTE.
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
#      DUNE_REPO=me/dune \
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

# Set try run to true to skip running mutating commands
DRY_RUN=${DRY_RUN:-"false"}

# run command if DRY_RUN is false or not set, else just print the command
function run_cmd () {
    if [[ "${DRY_RUN}" == "true" ]]; then
        echo "DRY RUN: $*"
    else
        "$@"
    fi
}

# run a precondition check, and if DRY_RUN is true, don't stop
# allowing every every unmet precondition to be reported in DRY_RUNS
function run_check () {
    if [[ "${DRY_RUN}" == "true" ]]; then
        ( "$@" ) || echo >&2 "DRY RUN: continuing despite the failure above"
    else
        "$@"
    fi
}


# Prompt for confirmation before running the irreversible release steps
function confirm () {
    local release_version="$1"
    read \
        -p "About to

  - cut ${RELEASE_KIND} ${release_version} on branch '${branch}'
  - push to '${DUNE_REMOTE}'
  - and publish via dune-release
  - with the changelog above.

Confirm? (y/Y) " \
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
DUNE_REPO=${DUNE_REPO:-"ocaml/dune"}
# Get the remote configured by envvar, or via the git config remote.pushDefault,
DUNE_REMOTE=${DUNE_REMOTE:-$(git config remote.pushdefault || echo "")}
# Finally fallback to 'origin' if the remote isn't configured
DUNE_REMOTE=${DUNE_REMOTE:-"origin"}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(realpath "${SCRIPT_DIR}/../../..")

# Check for required utilities
command -v git >/dev/null 2>&1 || err "script requires git"
command -v dune-release >/dev/null 2>&1 || err "script requires dune-release"
command -v gh >/dev/null 2>&1 || err "script requires gh"

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

# Rewrite the in-progress changelog section from the change fragments in
# doc/changes, replacing any section already there for that version.
function render_changelog () {
    local keep_fragments="$1" release_version="$2"
    strip_in_progress_section
    env "KEEP_FRAGMENTS=${keep_fragments}" \
        "${ROOT_DIR}/doc/changes/scripts/build_changelog.sh" "${release_version}"
}

# Refuse to release from a tree with uncommitted changes: the tarball is built
# from the tag, so local edits would be published silently or not at all.
function check_clean_worktree () {
    if [[ -n "$(git status --porcelain)" ]]; then
        err "the working tree has uncommitted changes"
    fi
}

# The packages declare their dune dependency from the language version in
# dune-project, so cutting X.Y.Z from a branch that still declares an older
# language version publishes packages with the wrong lower bound.
function check_version_consistency () {
    local series lang_version
    local bounds=()
    series=$(cut -d. -f1,2 <<< "${version}")
    lang_version=$(sed -n 's/^(lang dune \([0-9]*\.[0-9]*\))$/\1/p' \
        "${ROOT_DIR}/dune-project")
    if [[ -z "${lang_version}" ]]; then
        err "could not read the dune language version from dune-project"
    fi
    if [[ "${lang_version}" != "${series}" ]]; then
        err "dune-project declares (lang dune ${lang_version}) but ${version} is a ${series} release"
    fi
    mapfile -t bounds < <(sed -n 's/.*"dune" {>= "\([0-9.]*\)".*/\1/p' \
        "${ROOT_DIR}"/opam/*.opam | sort -u)
    if [[ "${#bounds[@]}" -ne 1 || "${bounds[0]}" != "${series}" ]]; then
        err "opam files declare dune lower bounds '${bounds[*]}' but ${version} is a ${series} release"
    fi
}

# dune-release escapes '~' as '_' when it creates the tag. An existing tag means
# this version was already cut, so publishing again would either fail or move a
# published tag.
function check_tag_unused () {
    local release_version="$1"
    local tag="${release_version//\~/_}"
    if git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
        err "tag ${tag} already exists locally; if a previous attempt failed part
way through, resume it with the individual make opam-release-<step> targets
rather than rerunning this script"
    fi
    if [[ -n "$(git ls-remote --tags "${DUNE_REMOTE}" "refs/tags/${tag}")" ]]; then
        err "tag ${tag} already exists on ${DUNE_REMOTE}"
    fi
}

# Publishing is not reversible, so the branch must be green first. Checks that
# have not reported are treated as a failure rather than as success: dispatching
# straight after a push would otherwise sail through an empty check list.
function check_ci_passed () {
    local sha endpoint total runs pending failed
    sha=$(git rev-parse HEAD)
    endpoint="repos/${DUNE_REPO}/commits/${sha}/check-runs"
    total=$(gh api "${endpoint}" --jq '.total_count') \
        || err "could not query the check runs for ${sha}"
    # The endpoint pages at 30 results, so without --paginate a pending or
    # failed check beyond the first page would read as success.
    runs=$(gh api --paginate "${endpoint}" \
        --jq '.check_runs[] | "\(.status)\t\(.conclusion)\t\(.name)"') \
        || err "could not query the check runs for ${sha}"
    if [[ -z "${runs}" ]]; then
        err "no checks have reported for ${sha}"
    fi
    # Guard against silently narrowing coverage if the pagination behaviour or
    # the response shape ever changes.
    if [[ "$(wc -l <<< "${runs}")" -ne "${total}" ]]; then
        err "expected ${total} check runs for ${sha} but read $(wc -l <<< "${runs}")"
    fi
    pending=$(awk -F'\t' '$1 != "completed" { print "  " $3 }' <<< "${runs}")
    if [[ -n "${pending}" ]]; then
        err "checks are still running for ${sha}:
${pending}"
    fi
    failed=$(awk -F'\t' \
        '$2 != "success" && $2 != "neutral" && $2 != "skipped" { print "  " $3 }' \
        <<< "${runs}")
    if [[ -n "${failed}" ]]; then
        err "checks did not pass for ${sha}:
${failed}"
    fi
}


# Render the changelog exactly as the release would and print the resulting
# diff, then restore the working tree. Fragments are always kept: a preview
# must not consume them, not even when previewing a full release.
function preview_changelog () {
    local release_version="$1"
    local changes="${ROOT_DIR}/CHANGES.md"
    (
        backup=$(mktemp "${changes}.XXXXXX") \
            || err "could not create a temporary file"
        cp "${changes}" "${backup}"
        trap 'mv -f "${backup}" "${changes}"' EXIT
        render_changelog true "${release_version}"
        echo
        # diff exits 1 when the files differ, which is the expected case here,
        # and 2 or more when it actually failed.
        status=0
        diff -u \
            --label "CHANGES.md (current)" \
            --label "CHANGES.md (after cutting ${release_version})" \
            "${backup}" "${changes}" || status=$?
        if (( status > 1 )); then
            err "could not diff the generated changelog"
        fi
        echo
    )
}

# The change fragments are the single source of truth: they are consumed
# (deleted) only for a full release, so successive prereleases regenerate the
# section in place from the accumulating fragments.
function update_changelog () {
    local release_version="$1"
    local keep_fragments=true
    if [[ "${RELEASE_KIND}" == "release" ]]; then
        keep_fragments=false
    fi
    run_cmd render_changelog "${keep_fragments}" "${release_version}"
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
    run_check check_clean_worktree
    run_cmd git pull --ff-only "${DUNE_REMOTE}" "${branch}"
    run_check check_version_consistency
    run_check check_tag_unused "${release_version}"
    run_check check_ci_passed
    preview_changelog "${release_version}"
    if [[ "${DRY_RUN:-false}" != "true" ]]; then
        confirm "${release_version}"
    fi
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
