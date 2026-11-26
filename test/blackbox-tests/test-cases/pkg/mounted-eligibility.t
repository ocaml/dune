Native eligibility belongs to the selected package's project, not its snapshot.
A package with no Dune rules initially uses its recipe.

  $ make_dune_project 3.25
  $ make_lockdir
  $ mkdir _eligibility
  $ printf '(lang dune 3.25)\n(package (name probe))\n' > _eligibility/dune-project
  $ make_lockpkg probe <<EOF
  > (version 1.0)
  > (source (copy $PWD/_eligibility))
  > (build (system "mkdir -p %{prefix}/share/probe; echo opaque > %{prefix}/share/probe/marker"))
  > EOF
  $ dune build _build/default/.pkg/probe/_install/share/probe/marker
  $ cat _build/default/.pkg/probe/_install/share/probe/marker
  opaque

Adding an unrelated nested project must not change that choice.

  $ mkdir _eligibility/unrelated
  $ printf '(lang dune 3.25)\n(package (name unrelated))\n' > _eligibility/unrelated/dune-project
  $ echo '(rule (target output) (action (write-file %{target} unrelated)))' > _eligibility/unrelated/dune
  $ dune build _build/default/.pkg/probe/_install/share/probe/marker
  $ cat _build/default/.pkg/probe/_install/share/probe/marker
  opaque

Rules in a subdirectory of the selected project do make it eligible.

  $ mkdir _eligibility/own
  $ echo '(rule (target output) (action (write-file %{target} native)))' > _eligibility/own/dune
  $ dune build _build/default/.pkg/probe/own/output
  $ cat _build/default/.pkg/probe/own/output
  native

