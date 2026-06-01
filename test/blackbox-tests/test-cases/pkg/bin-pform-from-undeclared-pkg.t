In Lock-kind contexts, [%{bin:X}] and [%{bin-available:X}] resolution falls
through to [Pkg_rules.which] via [Context.which]. [Pkg_rules.which] currently
scans every lockdir package's binary index, so any binary installed by any
locked package is discoverable from any stanza.

  $ make_lockdir

A lockdir package [provider] that installs a binary [mybin]:

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from provider
  >           "\| EOI
  >   )
  >  (system "chmod +x mybin")
  >  (system "echo 'bin: [ \"mybin\" ]' > provider.install")
  >  ))
  > EOF

With the current full-lockdir lookup, [mybin] is found and executed, without
the need for explicitly declaring any package dependencies:

  $ make_dune_project 3.24

  $ cat >dune <<'EOF'
  > (rule
  >  (alias test)
  >  (enabled_if %{bin-available:mybin})
  >  (action
  >   (progn
  >    (with-stdout-to mybin-output (run %{bin:mybin}))
  >    (with-stdout-to path-output
  >     (bash "echo $PATH")))))
  > EOF

  $ dune build @test

  $ cat _build/default/mybin-output
  from provider

The rule depends on the binary from the provider lockdir package:

  $ dune rules --format=json @test | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin/mybin"

The package's bin layout is added to $PATH:

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin
