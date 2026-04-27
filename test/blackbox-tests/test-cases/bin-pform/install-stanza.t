%{bin:NAME} for a binary installed via an (install) stanza, where
the install name differs from the source name.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF

  $ cat >script.sh <<'EOF'
  > #!/bin/sh
  > echo "hello from script"
  > EOF
  $ chmod +x script.sh

  $ cat >dune <<'EOF'
  > (install
  >  (package mypkg)
  >  (section bin)
  >  (files (script.sh as renamed)))
  > (rule
  >  (deps %{bin:renamed})
  >  (action
  >   (with-stdout-to out (bash "renamed"))))
  > EOF

The action invokes the install-renamed binary via PATH:

  $ dune build out
  $ cat _build/default/out
  hello from script

The bin-layout symlink uses the install name:

  $ dune rules --format=json _build/default/out \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' | censor
  "_build/default/script.sh"
  "_build/install/default/.binaries/$DIGEST/renamed"
