A lock-dir package depends on a workspace package that provides a
public executable.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-tool))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name main)
  >  (package ws-tool)
  >  (public_name ws-tool))
  > EOF
  $ cat > src/main.ml <<EOF
  > let () = print_endline "Hello from ws-tool!"
  > EOF

The lock dir contains one package "consumer" that declares "ws-tool"
as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends ws-tool)
  > (build (run ws-tool))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The locked recipe executes the materialised workspace tool.

  $ dune build out
  Hello from ws-tool!
  $ echo 'let () = print_endline "updated tool"' > src/main.ml
  $ dune build out
  updated tool

An opaque workspace provider supplies cookie variables and exported environment
through the same materialiser, including its recipe's version override.

  $ echo '(using unreleased 0.1)' >> dune-project
  $ cat > src/dune <<'EOF'
  > (opam
  >  (package ws-tool)
  >  (dir build-src)
  >  (version 2.0)
  >  (build
  >   (system "set -eu; printf '#!/bin/sh\\necho opaque-tool\\n' > tool; chmod +x tool; echo 'bin: [ \"tool\" {\"ws-tool\"} ]' > ws-tool.install; echo 'opam-version: \"2.0\" variables { answer: \"42\" }' > ws-tool.config"))
  >  (exported_env (= WORKSPACE_ANSWER %{pkg-self:answer})))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends ws-tool)
  > (build
  >  (progn
  >   (system "test '%{pkg:ws-tool:answer}' = 42 && test '%{pkg:ws-tool:version}' = 2.0 && test \"$WORKSPACE_ANSWER\" = 42")
  >   (run ws-tool)))
  > EOF
  $ dune build out
  opaque-tool

Preparation must not treat a workspace variable as absent, or build the workspace
provider while choosing a builder. This substitution needs the provider's cookie,
so the prepared source stays with opaque execution despite being a Dune project.

  $ mkdir _consumer
  $ cat > _consumer/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name consumer))
  > EOF
  $ echo '(rule (target sentinel) (action (write-file %{target} native)))' > _consumer/dune
  $ echo '%{ws-tool:answer}%' > _consumer/answer.in
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/_consumer))
  > (depends ws-tool)
  > (build (progn (substitute answer.in answer) (run cat answer)))
  > EOF
  $ dune build out
  42
  $ test ! -e "$pkg_root/consumer/sentinel"

Making the workspace recipe depend back on the locked recipe creates a real
install/build cycle. Report it as a dependency error, not a missing lock entry.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name ws-tool) (depends consumer) (allow_empty))
  > EOF
  $ dune build out 2>error
  [1]
  $ grep '^Error:' error
  Error: Dependency cycle between packages:
