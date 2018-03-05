#!/bin/bash

# CR-someday jdimino: maybe it's possible to get cmdliner to print that directly

set -e -o pipefail

jbuilder=$1

CMDS=$($jbuilder --help=plain | \
           sed -n '/COMMANDS/,/OPTIONS/p' | sed -En 's/^       ([a-z-]+)/\1/p')

for cmd in $CMDS; do
    cat <<EOF

(rule
 ((targets (jbuilder-$cmd.1))
  (action  (with-stdout-to \${@}
            (run \${bin:jbuilder} $cmd --help=groff)))))

(install
 ((section man)
  (files (jbuilder-$cmd.1))))
EOF
done

echo
