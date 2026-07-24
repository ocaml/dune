A "mixed" transitive dependency chain [p] -> [q] -> [r] has one edge crossing
the workspace/lockdir boundary. The two directions behave differently today, and
both are closed once the dependency closure is unified across workspace and
lockdir edges (the in-out work).

[p] (workspace) -> [q] (workspace) -> [r] (lockdir). The [q] -> [r] edge is
allowed, so this builds; but the dependency closures currently don't cross the
workspace/lockdir boundary, so [r]'s binary is narrowed out unless [r] is
declared directly on [p]. A unified dependency closure from the in-and-out work
will fix this.

  $ make_lockdir

A lockdir package [r] that installs [r-tool]:

  $ make_lockpkg r <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo '#!/bin/sh' > r-tool")
  >   (system "echo 'echo from r' >> r-tool")
  >   (system "chmod +x r-tool")
  >   (system "echo 'bin: [ \"r-tool\" ]' > r.install")))
  > EOF

  $ mkdir -p p q
  $ cat >p/dune <<'EOF'
  > (rule (with-stdout-to r-avail (echo %{bin-available:r-tool})))
  > EOF

[p] depends only on the workspace sibling [q]; [q] depends on the lockdir
package [r]:

  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (package (name p) (allow_empty) (dir p) (depends q))
  > (package (name q) (allow_empty) (dir q) (depends r))
  > EOF

  $ dune build p/r-avail

[r-tool] is NOT resolved. [r] is only reachable transitively through the
workspace package [q], but workspace bins and lockdir bins are resolved via two
separate dependency closures that don't cross the workspace/lockdir boundary, so
[r] is not in [p]'s lockdir closure.

  $ cat _build/default/p/r-avail
  false

Declaring [r] directly on [p] works. This is a workaround until the unified
dependency closure lands from the in-and-out work:

  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (package (name p) (allow_empty) (dir p) (depends q r))
  > (package (name q) (allow_empty) (dir q) (depends r))
  > EOF
  $ dune clean
  $ dune build p/r-avail
  $ cat _build/default/p/r-avail
  true

The two blocks differ under narrowing: the transitive-only edge (block 1) is
narrowed out (false); declaring [r] directly (block 2) resolves it (true).
Before narrowing both were true, so block 1 looked redundant.

[p] (workspace) -> [q] (lockdir) -> [r] (workspace). Now the [q] -> [r] edge is
a lockdir package depending on a workspace package. This is a lock-VALIDATION
restriction, not a narrowing case: it is rejected before any binary resolution
runs, so narrowing does not change it -- it flips only when the in-out work
lifts the restriction (see [../lockdir-workspace-deps/basic.t] for the bare
rejection), at which point [p] should resolve [r-tool], since [r] is then in
[p]'s transitive closure.

  $ rm -rf p q r dune.lock
  $ dune clean

  $ make_lockdir
  $ make_lockpkg q <<'EOF'
  > (version 0.0.1)
  > (depends r)
  > (build (system "true"))
  > EOF

  $ mkdir -p p r
  $ cat >r/r-tool.sh <<'EOF'
  > #!/bin/sh
  > echo from r
  > EOF
  $ chmod +x r/r-tool.sh
  $ cat >r/dune <<'EOF'
  > (install (package r) (section bin) (files (r-tool.sh as r-tool)))
  > EOF
  $ cat >p/dune <<'EOF'
  > (rule (with-stdout-to r-avail (echo %{bin-available:r-tool})))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (package (name p) (allow_empty) (dir p) (depends q))
  > (package (name r) (allow_empty) (dir r))
  > EOF

  $ dune build p/r-avail 2>&1
  File "_build/_private/default/.lock/dune.lock/q.pkg", line 2, characters
  9-10:
  The package "q" depends on the package "r", but "r" does not appear in the
  lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]
