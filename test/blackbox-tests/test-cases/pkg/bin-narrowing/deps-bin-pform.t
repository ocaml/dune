Naming a binary as [(deps %{bin:X})] does two things: the pform is expanded as
an ordinary file dependency, and X is also staged into a [.binaries] directory
that is prepended to the action's $PATH. So an action can invoke X by bare
name, without going through [%{bin:X}] again.

This test records how X is resolved in three configurations. See
[workspace-deps.t] for the plain [%{bin:X}] side.

  $ make_lockdir

Sibling that is not a declared dependency
-----------------------------------------

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

Use the same provider and workspace consumers through both backends, in fresh
roots. The workspace sibling is not declared by the consumer, so neither
lookup may select its competing binary.

  $ mkdir -p _provider _selection/sibling _selection/consumer native opaque
  $ (cd _provider && make_dune_project_with_package 3.25 provider)
  $ echo '(install (section bin) (files (dup.sh as dup)))' > _provider/dune
  $ printf '#!/bin/sh\necho from lockdir\n' > _provider/dup.sh
  $ printf '#!/bin/sh\necho from workspace\n' > _selection/sibling/dup.sh
  $ chmod +x _provider/dup.sh _selection/sibling/dup.sh
  $ cat > _selection/sibling/dune <<'EOF'
  > (install (package sibling) (section bin) (files (dup.sh as dup)))
  > EOF
  $ cat > _selection/consumer/dune <<'EOF'
  > (rule
  >  (action (with-stdout-to via-pform (run %{bin:dup}))))
  > (rule
  >  (deps %{bin:dup})
  >  (action (with-stdout-to via-path (system dup))))
  > EOF
  $ cat > _selection/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name sibling) (allow_empty) (dir sibling))
  > (package (name consumer) (allow_empty) (dir consumer) (depends provider))
  > EOF
  $ cp -R _selection/. native/
  $ cp -R _selection/. opaque/
  $ make_backend_lockdirs provider "$PWD/_provider"

The native provider is selected without running its failing recipe. Pform
execution and the independent dependency-pform/PATH action both select it.

  $ (cd native && dune build consumer/via-pform consumer/via-path)
  $ cat native/_build/default/consumer/via-pform \
  >   native/_build/default/consumer/via-path
  from lockdir
  from lockdir
  $ test ! -e native/_build/default/.pkg/provider/_install/cookie

The opaque provider gives the same selection result. It is not a workspace
binary to stage in [.binaries]; [(system dup)] finds the installed copy on
[provider]'s PATH. The marker and cookie prove that its recipe really ran.

  $ (cd opaque && dune build consumer/via-pform consumer/via-path)
  $ cat opaque/_build/default/consumer/via-pform \
  >   opaque/_build/default/consumer/via-path
  from lockdir
  from lockdir
  $ cat opaque/_build/default/.pkg/provider/_install/opaque-recipe
  opaque
  $ test -f opaque/_build/default/.pkg/provider/_install/cookie

In [workspace-shadows-lockdir.t] the workspace binary does shadow the lockdir
one: there it belongs to the directory's own owning package, whose binaries are
always visible to it.

A name installed by two workspace packages
-------------------------------------------

This workspace-only case is independent of the competing locked providers.

  $ mkdir workspace
  $ cd workspace
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
  >  (action (with-stdout-to via-pform (run %{bin:dup}))))
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

The ordinary narrowed pform lookup selects [pkg-a], the only provider
among the packages listed in [pkg-c]'s dependencies:

  $ dune build c/via-pform
  $ cat _build/default/c/via-pform
  from a

[pkg-c] depends on [pkg-a] only, and [pkg-a]'s [dup] binary is picked up
correctly, via PATH too.

  $ dune build c/via-path

  $ cat _build/default/c/via-path
  from a
