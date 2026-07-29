Naming a binary as [(deps %{bin:X})] does two things: the pform is expanded as
an ordinary file dependency, and X is also staged into a [.binaries] directory
that is prepended to the action's $PATH ([Bin_layout], reached from
[Dep_conf_eval.make_bin_env]). So an action can invoke X by bare name, without
going through [%{bin:X}] again.

This test records how X is resolved in three configurations. See
[workspace-deps.t] for the plain [%{bin:X}] side.

  $ make_lockdir

A sibling that is not a declared dependency
-------------------------------------------

[producer] installs [producer-bin]; [consumer] names it in [(deps ...)] and
invokes it by bare name through the shell, without declaring a dependency on
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
  >  (deps %{bin:producer-bin})
  >  (action (with-stdout-to via-deps (system producer-bin))))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name producer) (allow_empty) (dir producer))
  > (package (name consumer) (allow_empty) (dir consumer))
  > EOF

The [%{bin:...}] in the [(deps ...)] field is expanded through the directory's
artifacts like any other pform, so it is narrowed and the rule fails before
[producer-bin] is staged. Nothing reaches $PATH:

  $ dune build consumer/via-deps
  File "consumer/dune", line 2, characters 7-26:
  2 |  (deps %{bin:producer-bin})
             ^^^^^^^^^^^^^^^^^^^
  Error: Program producer-bin not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "producer-bin" to this
  package
  [1]
  $ cat _build/default/consumer/via-deps
  cat: _build/default/consumer/via-deps: No such file or directory
  [1]

A declared lockdir binary and an undeclared sibling of the same name
---------------------------------------------------------------------

  $ rm -rf producer consumer dune.lock
  $ dune clean

  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > dup")
  >   (system "echo 'echo from lockdir' >> dup")
  >   (system "chmod +x dup")
  >   (system "echo 'bin: [ \"dup\" ]' > provider.install")))
  > EOF

  $ mkdir -p sibling consumer
  $ cat >sibling/dup.sh <<'EOF'
  > #!/bin/sh
  > echo from workspace
  > EOF
  $ chmod +x sibling/dup.sh
  $ cat >sibling/dune <<'EOF'
  > (install (package sibling) (section bin) (files (dup.sh as dup)))
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (rule
  >  (action (with-stdout-to via-pform (run %{bin:dup}))))
  > (rule
  >  (deps %{bin:dup})
  >  (action (with-stdout-to via-path (system dup))))
  > EOF

[consumer] declares the lockdir package [provider], but not the workspace
sibling [sibling]:

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name sibling) (allow_empty) (dir sibling))
  > (package (name consumer) (allow_empty) (dir consumer) (depends provider))
  > EOF

  $ dune build consumer/via-pform consumer/via-path

[sibling] is not in [consumer]'s dependency closure, so its [dup] is narrowed
out of [local_bins] and the lookup falls through to the declared lockdir copy.
The staged copy on $PATH agrees with the pform, so [(system dup)] and
[%{bin:dup}] name the same file.

In [workspace-shadows-lockdir.t] the workspace binary does shadow the lockdir
one: there it belongs to the directory's own owning package, which is always in
its own closure.

  $ cat _build/default/consumer/via-pform
  from lockdir
  $ cat _build/default/consumer/via-path
  from lockdir

A name installed by two workspace packages
-------------------------------------------

  $ rm -rf sibling consumer dune.lock
  $ dune clean

  $ make_lockdir
  $ mkdir -p a b c
  $ cat >a/dup.sh <<'EOF'
  > #!/bin/sh
  > echo from a
  > EOF
  $ cat >b/dup.sh <<'EOF'
  > #!/bin/sh
  > echo from b
  > EOF
  $ chmod +x a/dup.sh b/dup.sh
  $ cat >a/dune <<'EOF'
  > (install (package pkg-a) (section bin) (files (dup.sh as dup)))
  > EOF
  $ cat >b/dune <<'EOF'
  > (install (package pkg-b) (section bin) (files (dup.sh as dup)))
  > EOF
  $ cat >c/dune <<'EOF'
  > (rule
  >  (deps %{bin:dup})
  >  (action (with-stdout-to via-path (system dup))))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name pkg-a) (allow_empty) (dir a))
  > (package (name pkg-b) (allow_empty) (dir b))
  > (package (name pkg-c) (allow_empty) (dir c) (depends pkg-a))
  > EOF

[pkg-c] depends on [pkg-a], so resolving [dup] in [c/] is unambiguous and
[Bin_layout.create] stages [pkg-a]'s copy. But the staged binary is symlinked
from a [.binaries] directory keyed only by the names being staged, and shared
between every directory staging the same names, so
[Bin_layout.symlink_rules_for_key] resolves [dup] again there -- in a directory
with no owning package, and through the context-wide artifacts -- where both
definitions are in scope:

  $ dune build c/via-path
  File "b/dune", line 1, characters 47-53:
  1 | (install (package pkg-b) (section bin) (files (dup.sh as dup)))
                                                     ^^^^^^
  Error: binary "dup" is available from more than one definition. It is also
  available in:
  - a/dune:1
  [1]
