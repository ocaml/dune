Demonstrate the shell field in the cram stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ printShell() {
  > dune trace cat | jq 'select(.cat == "process" and (.args.categories | index("cram"))) | .args | .prog | split("/") | last'
  > }

  $ cat >foo.t <<'EOF'
  >   $ echo foo
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]
  $ printShell
  "sh"

  $ cat > dune <<EOF
  > (cram
  >  (shell bash))
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]
  $ printShell
  "bash"

  $ cat >options.t <<'EOF'
  >   $ set -o | grep -e pipefail -e nounset -e errexit
  >   errexit        	on
  >   nounset        	on
  >   pipefail       	on
  > EOF

  $ dune runtest options.t
