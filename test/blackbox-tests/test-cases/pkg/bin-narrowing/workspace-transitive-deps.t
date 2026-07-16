Workspace-installed binaries (the local_bins in [Artifacts]) are currently not
narrowed: a package's stanzas resolve any workspace package's binary, including
packages not in its dependency closure.

Once narrowing lands, this will be restricted to the owning package's TRANSITIVE
workspace dependency closure -- like [transitive-deps.t] for the lockdir side. In
particular a transitively-depended package's binary must still resolve (which is
what distinguishes narrowing to the transitive closure from narrowing to only the
direct dependencies).

  $ make_lockdir

Three workspace packages forming a chain [p] -> [q] -> [r]. [q] installs
[q-tool] and [r] installs [r-tool]. A sibling [s] installs [s-tool] and is not
in [p]'s dependency closure.

  $ mkdir -p p q r s
  $ cat >q/q-tool.sh <<'EOF'
  > #!/bin/sh
  > echo from q
  > EOF
  $ cat >r/r-tool.sh <<'EOF'
  > #!/bin/sh
  > echo from r
  > EOF
  $ cat >s/s-tool.sh <<'EOF'
  > #!/bin/sh
  > echo from s
  > EOF
  $ chmod +x q/q-tool.sh r/r-tool.sh s/s-tool.sh
  $ cat >q/dune <<'EOF'
  > (install (package q) (section bin) (files (q-tool.sh as q-tool)))
  > EOF
  $ cat >r/dune <<'EOF'
  > (install (package r) (section bin) (files (r-tool.sh as r-tool)))
  > EOF
  $ cat >s/dune <<'EOF'
  > (install (package s) (section bin) (files (s-tool.sh as s-tool)))
  > EOF
  $ cat >p/dune <<'EOF'
  > (rule (with-stdout-to q-avail (echo %{bin-available:q-tool})))
  > (rule (with-stdout-to r-avail (echo %{bin-available:r-tool})))
  > (rule (with-stdout-to s-avail (echo %{bin-available:s-tool})))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (package (name p) (allow_empty) (dir p) (depends q))
  > (package (name q) (allow_empty) (dir q) (depends r))
  > (package (name r) (allow_empty) (dir r))
  > (package (name s) (allow_empty) (dir s))
  > EOF

  $ dune build @all
[q-tool] (direct dep p -> q) is available:

  $ cat _build/default/p/q-avail
  true

[r-tool] (transitive dep p -> q -> r) is available:

  $ cat _build/default/p/r-avail
  true

[s-tool] (sibling not in p's closure) is available too, since workspace binaries
are not narrowed yet:

  $ cat _build/default/p/s-avail
  true
