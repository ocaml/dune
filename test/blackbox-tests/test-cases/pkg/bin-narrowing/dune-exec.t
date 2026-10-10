[dune exec] resolves a managed executable using the artifacts of the current
directory. The launched process should receive the matching lock-directory PATH
so that it can invoke companion executables from its installed environment.

  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > helper <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from helper
  >           "\| EOI
  >   )
  >   (system "\| cat > launcher <<'EOI'
  >           "\| #!/bin/sh
  >           "\| if command -v helper >/dev/null; then
  >           "\|   helper
  >           "\| else
  >           "\|   echo helper is missing from PATH
  >           "\|   exit 1
  >           "\| fi
  >           "\| EOI
  >   )
  >   (system "chmod +x helper launcher")
  >   (system "echo 'bin: [ \"helper\" \"launcher\" ]' > provider.install")
  >  ))
  > EOF

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (action (with-stdout-to action-output (run %{bin:launcher}))))
  > EOF

A normal rule gets its directory's [Env_node] environment, so [launcher] can
find [helper] by bare name:

  $ dune build action-output
  $ cat _build/default/action-output
  from helper

The helper is also installed and directly resolvable by [dune exec]:

  $ dune exec helper
  from helper

[dune exec launcher] is also able to its installed companion [helper]:

  $ dune exec launcher
  from helper

[dune exec launcher] should print [from helper], just like the rule. Its process
environment must use the same directory-specific package closure that was used
to resolve [launcher].
