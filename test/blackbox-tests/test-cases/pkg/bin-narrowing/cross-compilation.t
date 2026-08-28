Lock-directory binaries used by cross-compiled actions must come from the host
context. The pform lookup already uses host artifacts; this test compares it
with bare-name lookup through the action's PATH.

  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > tool <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from provider
  >           "\| EOI
  >   )
  >   (system "chmod +x tool")
  >   (system "echo 'bin: [ \"tool\" ]' > provider.install")
  >  ))
  > EOF

Create explicit host and target contexts, and enable package management for the
workspace:

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF
  $ cat >dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default))
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

The target-context rule both runs the pform and looks up the same name on PATH.
The explicit dependency builds the host package selected by [%{bin:tool}].

  $ cat >dune <<'EOF'
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (deps %{bin:tool})
  >  (action
  >   (progn
  >    (with-stdout-to pform-output (run %{bin:tool}))
  >    (with-stdout-to path-output
  >     (bash "if command -v tool >/dev/null; then tool; else echo tool is missing from PATH; fi"))
  >    (with-stdout-to env-output (bash "echo $PATH")))))
  > EOF

The pform resolves and executes the host artifact:

  $ dune build _build/target/pform-output
  $ cat _build/target/pform-output
  from provider

The rule dependency confirms that the selected binary belongs to the host
context:

  $ dune rules --format=json _build/target/pform-output | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/_private/host/.pkg/provider.0.0.1-$DIGEST/target/bin/tool"

Bare-name lookup succeeds too:

  $ cat _build/target/path-output
  from provider

The per-directory PATH points at the host context:

  $ env_added "$(cat _build/target/env-output)" "$PATH" | censor
  $PWD/_build/_private/host/.pkg/provider.0.0.1-$DIGEST/target/bin
