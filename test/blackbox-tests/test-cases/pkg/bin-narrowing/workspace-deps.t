Workspace-installed binaries are narrowed: a package's stanzas resolve a
sibling package's binary only when it is declared as a dependency. This would
match what the package would see built in isolation (opam-repo-ci), where an
undeclared sibling isn't installed. See [lockdir-deps.t] for the lockdir side.

  $ make_lockdir

Two workspace packages, each owning a subdirectory. [producer] installs a
binary [producer-bin]; [consumer] uses it WITHOUT declaring a dependency on
[producer].

  $ mkdir -p producer consumer
  $ cat >producer/producer-bin.sh <<'EOF'
  > #!/bin/sh
  > echo "hello from producer"
  > EOF
  $ chmod +x producer/producer-bin.sh
  $ cat >producer/dune <<'EOF'
  > (install
  >  (package producer)
  >  (section bin)
  >  (files (producer-bin.sh as producer-bin)))
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (rule
  >  (with-stdout-to producer-available (echo %{bin-available:producer-bin})))
  > (rule
  >  (with-stdout-to producer-run (run %{bin:producer-bin})))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name producer) (allow_empty) (dir producer))
  > (package (name consumer) (allow_empty) (dir consumer))
  > EOF

  $ dune build @all
  File "consumer/dune", line 4, characters 35-54:
  4 |  (with-stdout-to producer-run (run %{bin:producer-bin})))
                                         ^^^^^^^^^^^^^^^^^^^
  Error: Program producer-bin not found in the tree or in PATH
   (context: default)
  Hint: "producer-bin" is not provided by any dependency of this directory's
  package. Add a dependency on the package that provides it.
  [1]

The workspace binary is not available since [consumer] does not depend on
[producer]:

  $ cat _build/default/consumer/producer-available
  false

And the run fails:

  $ cat _build/default/consumer/producer-run
  cat: _build/default/consumer/producer-run: No such file or directory
  [1]

Adding it as a dependency should fix the resolution and the build.

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name producer) (allow_empty) (dir producer))
  > (package (name consumer) (allow_empty) (dir consumer) (depends producer))
  > EOF

  $ dune build @all

The workspace binary is now available since [consumer] depends on [producer]:

  $ cat _build/default/consumer/producer-available
  true

And it runs:

  $ cat _build/default/consumer/producer-run
  hello from producer
