%{bin:NAME} for a binary installed via an (install) stanza, where
the install name differs from the source name.

  $ make_dune_project_with_package 3.24 mypkg

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

The .binaries symlink uses the install name:

  $ dune rules --format=json _build/default/out \
  >   | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/default/script.sh"
  "_build/install/default/.binaries/$DIGEST/renamed"
