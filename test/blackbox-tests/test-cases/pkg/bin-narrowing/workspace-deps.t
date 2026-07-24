Currently there is no narrowing of workspace-installed binaries: a package's
stanzas resolve a sibling package's binary even without declaring a dependency
on it.

Once narrowing lands, this will be restricted to the owning package's declared
(transitive) dependency closure matching what the package would see built in
isolation (opam-repo-ci), where an undeclared sibling isn't installed. See
[lockdir-deps.t] for the lockdir side.

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
Even though [consumer] does not depend on [producer], the workspace binary is
available (workspace binaries are not narrowed):

  $ cat _build/default/consumer/producer-available
  true

And it runs:

  $ cat _build/default/consumer/producer-run
  hello from producer
