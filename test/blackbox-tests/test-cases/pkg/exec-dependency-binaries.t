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

Without a package scope, dune exec must not build arbitrary opaque packages to
look for its argument:

  $ dune exec mybin
  Error: Program 'mybin' not found!
  [1]
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

Declaring the dependency in a directory scope makes it available:

  $ cat >>dune-project <<'EOF'
  > (package
  >  (name consumer)
  >  (allow_empty)
  >  (dir .)
  >  (depends foo))
  > EOF
  $ dune exec mybin
  from foo

  $ dune exec -- mybin arg1 arg2
  from foo arg1 arg2

  $ mkdir sub
  $ cd sub && dune exec --root .. mybin
  from foo
