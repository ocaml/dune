Binaries installed by lock-dir dependencies can be run with `dune exec`.

  $ make_lockdir
  $ make_lockpkg foo <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from foo "$@"
  >           "\| EOI
  >   )
  >   (system "chmod +x mybin")
  >   (system "echo 'bin: [ \"mybin\" ]' > foo.install")))
  > EOF
  $ make_dune_project 3.24

  $ command -v mybin || echo "mybin not on PATH"
  mybin not on PATH

Without a package scope, dune exec looks for its argument in the opaque
packages, building them:

  $ dune exec mybin
  from foo

Declaring the dependency in a directory scope keeps it available:

  $ echo '(package (name consumer) (allow_empty) (dir .) (depends foo))' >> dune-project
  $ dune exec mybin
  from foo

  $ dune exec -- mybin arg1 arg2
  from foo arg1 arg2

  $ mkdir sub
  $ cd sub && dune exec --root .. mybin
  from foo
