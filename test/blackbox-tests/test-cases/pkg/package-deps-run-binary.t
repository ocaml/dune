A single action that both declares [(deps (package X))] and resolves a program
installed by that same package [X] (via [(run ...)] or [%{bin:...}]) should run
correctly under the default sandbox.

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

The package dependency and the binary dependency can be sandboxed together.

  $ dune build @all
  $ cat _build/default/out
  from provider

Resolving the binary without a package dependency still works.

  $ cat >dune <<'EOF'
  > (rule
  >  (action
  >   (with-stdout-to out (run mybin))))
  > EOF

  $ dune build @all 2>&1 | censor
  
  $ cat _build/default/out
  from provider
