A single action that both declares [(deps (package X))] and resolves a program
installed by that same package [X] (via [(run ...)] or [%{bin:...}]) should run
correctly under both default and copy sandboxing.

  $ export DUNE_CACHE=disabled
  $ make_lockdir

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from provider
  >           "\| EOI
  >   )
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > provider.install")
  >  ))
  > EOF

  $ make_dune_project 3.25

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package provider))
  >  (action
  >   (with-stdout-to out (run mybin))))
  > EOF

With default sandboxing, the overlapping package-directory and executable
dependencies are both available to the action.

  $ dune build @all
  $ cat _build/default/out
  from provider

CR: Copy mode tries to populate the package executable twice and fails.
The package-directory and executable dependencies still need deduplication.
Prefer no sandbox for internal rules, then copy for user rules that require
sandboxing. Remove any existing output to force the action to run again.
Normalize the copier-specific error wrapper while preserving the diagnostic.

  $ cat > copy-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none copy)
  > EOF
  $ test ! -e _build/default/out || rm _build/default/out
  $ dune build @all --config-file copy-config 2>&1 \
  > | sed -E \
  >     -e '/^Error.*:$/ { N; s/\n/ /; }' \
  >     -e 's/(open|copyfile)\((.*)\): File exists$/\2: File exists/' \
  > | censor
  Error: _build/.sandbox/$DIGEST1/_private/default/.pkg/provider.0.0.1-$DIGEST2/target/bin/mybin: File exists
  -> required by _build/default/out
  -> required by alias all
  [1]

Without [(deps (package provider))], both sandbox modes should succeed.

  $ cat >dune <<'EOF'
  > (rule
  >  (action
  >   (with-stdout-to out (run mybin))))
  > EOF

  $ dune build @all
  $ cat _build/default/out
  from provider

  $ rm _build/default/out
  $ dune build @all --config-file copy-config
  $ cat _build/default/out
  from provider
