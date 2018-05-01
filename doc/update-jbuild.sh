#!/bin/bash

# CR-someday jdimino: maybe it's possible to get cmdliner to print that directly

set -e -o pipefail

CMDS=$(dune --help=plain | \
           sed -n '/COMMANDS/,/OPTIONS/p' | sed -En 's/^       ([a-z-]+)/\1/p')

for cmd in $CMDS; do
    cat <<EOF

(rule
 ((targets (dune-$cmd.1))
  (action  (with-stdout-to \${@}
            (run dune $cmd --help=groff)))))

(install
 ((section man)
  (files (dune-$cmd.1))))
EOF
done

echo
