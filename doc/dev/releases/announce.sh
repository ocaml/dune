#!/usr/bin/env sh

# FIXME
OCAML_ORG_REPO="${HOME}/Repos/ocaml/ocaml.org"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT="${SCRIPT_DIR}/../../"
CHANGES="${ROOT}/CHANGES.md"

set -ue
set -o pipefail

is_patch_release() {
  local v="$1"

  # Match standard semantic versioning format and capture the patch segment
  if [[ "$v" =~ ^[0-9]+\.[0-9]+\.([0-9]+) ]]; then
    local patch="${BASH_REMATCH[1]}"
    
    # Evaluate if the patch integer is greater than 0
    if [[ "$patch" -gt 0 ]]; then
      return 0  # Success (True: it is a patch release)
    else
      return 1  # Failure (False: it is a major or minor release)
    fi
  else
    echo "Error: Invalid semantic version format." >&2
    return 2    # Error state
  fi
}

version=$(awk '
  # Match lines starting with a standard semantic versioning pattern
  /^[0-9]+\.[0-9]+\.[0-9]+/ {
      # Print the first field (the version string itself)
      print $1
      # Terminate execution immediately
      exit
  }
  ' "$CHANGES")

# version_pat='[0-9]+\.[0-9]+\.[0-9]+.*'
changelog=$(awk '
  # Detect release header (e.g., "3.21.1 (2026-02-10)")
  /^[0-9]+\.[0-9]+/ {
      if (!latest_found) {
          # Enter the latest release block
          latest_found = 1
          in_latest = 1
          next
      } else {
          # Exit immediately when the second release is encountered
          exit
      }
  }
  
  # Detect ### sections within the latest release
  /^### / {
      if (in_latest) {
          in_section = 1
      }
  }
  
  # Print line if we are inside a ### section of the latest release
  {
      if (in_latest && in_section) {
          print "    " $0
      }
  }
  ' "$CHANGES"
)

announcement() {
    echo """
---
title: Dune ${version}
tags:
  - dune
  - platform
authors:
contributors:
versions:
experimental: false
ignore: false
github_release_tags:
  - ${version}
changelog: |
${changelog}

---

The Dune team is pleased to announce [the release of dune
${version}](https://github.com/ocaml/dune/releases/tag/${version}).
"""

    if is_patch_release "$version"
    then

        echo """This is a patch release consisting of bug fixes. See [the full
changelog](https://github.com/ocaml/dune/releases/tag/${version}) for all the
changes and for attribution to the contributors who made it all possible. Thank
you, contributors!

If you encounter a problem with this release, please report it in [our issue
tracker](https://github.com/ocaml/dune/issues).
"""

    else

        echo """The Dune team is pleased to announce [the release of dune
${version}](https://github.com/ocaml/dune/releases/tag/${version}).

See [the full changelog](https://github.com/ocaml/dune/releases/tag/${version})
for all new features and fixes, and for attribution to the contributors who made
it all possible. Thank you, contributors!

If you encounter a problem with this release, please report it in [our issue
tracker](https://github.com/ocaml/dune/issues).
"""

    fi
}
DATE=$(date "+%Y-%m-%d")
announcement > "${OCAML_ORG_REPO}/data/platform_releases/dune/${DATE}-dune.${version}.md"
