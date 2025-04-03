#!/usr/bin/env bash
set -euo pipefail

# Setup test environment
WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT
cd "$WORKDIR"

# Create test files
cat > dune-project <<'EOF'
(lang dune 3.0)
EOF

cat > dune <<'EOF'
(rule
 (targets output.txt secondary.log)
 (action
  (progn
   (write-file output.txt "Primary content")
   (write-file secondary.log "Log content"))))

(rule
 (alias runtest)
 (action
  (progn
   (diff output.txt output.expected)
   (diff secondary.log secondary.expected))))
EOF

# Create expected files - MUST match exactly what's generated
printf "Primary content" > output.expected
printf "Log content" > secondary.expected

# Run the test
dune build @runtest