(deps (package NAME)) in a cross-compilation context. The install
layout dir should sit under the host context's install tree.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.24)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

  $ mkdir src
  $ cat >src/dune <<EOF
  > (library (public_name mypkg))
  > EOF
  $ cat >src/mypkg.ml <<EOF
  > let x = 1
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (deps (package mypkg))
  >  (action
  >   (with-stdout-to out (echo "ok"))))
  > EOF

  $ dune build _build/target/out

The layout dir should be under install/host/ (the layout for a
target-context action uses host artifacts):

  $ dune rules --format=json _build/target/out \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep dune-package | censor
  "_build/install/host/.packages/$DIGEST/lib/mypkg/dune-package"
