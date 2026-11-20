Cinaps formats dune and dune-project corrections only when Dune formatting is
enabled. The extension-version boundary and global formatting disablement are
covered separately in format-generated.t.

Cinaps needs an explicit comment syntax for these extensionless filenames:

  $ cinaps=$(command -v cinaps)
  $ mkdir bin
  $ printf '#!/bin/sh\nexec "%s" -syntax ocaml "$@"\n' "$cinaps" > bin/cinaps
  $ chmod +x bin/cinaps
  $ export PATH="$PWD/bin:$PATH"

  $ make_project() {
  >   make_cinaps_project 3.26 1.4
  >   echo "(formatting (enabled_for $1))" >> dune-project
  >   cat >> dune-project <<'EOF'
  > ; (*$ print_string "\n(name    generated)\n; " *)
  > ; (*$*)
  > EOF
  >   cat > dune <<'EOF'
  > (cinaps (files dune dune-project))
  > ; (*$ print_string "\n(rule   (alias generated))\n; " *)
  > ; (*$*)
  > EOF
  > }

Enabling OCaml formatting does not enable formatting for either Dune file:

  $ mkdir ocaml-only
  $ cd ocaml-only
  $ make_project ocaml
  $ dune build @cinaps --auto-promote > output 2>&1
  [1]
  $ grep '^(rule' dune
  (rule   (alias generated))
  $ grep '^(name' dune-project
  (name    generated)

Enabling Dune formatting formats both files, without enabling OCaml formatting:

  $ cd ..
  $ mkdir dune-only
  $ cd dune-only
  $ make_project dune
  $ dune build @cinaps --auto-promote > output 2>&1
  [1]
  $ grep -A 1 '^(rule' dune
  (rule
   (alias generated))
  $ grep '^(name' dune-project
  (name generated)
