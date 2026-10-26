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

Copy mode must provide the same inputs without copying the executable twice.
Prefer no sandbox for internal rules, then copy for user rules that require
sandboxing. Remove the existing output to force the action to run again.

  $ cat > copy-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none copy)
  > EOF
  $ rm _build/default/out
  $ dune build @all --config-file copy-config
  $ cat _build/default/out
  from provider

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
