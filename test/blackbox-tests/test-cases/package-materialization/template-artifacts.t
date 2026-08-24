A META file template can override path-bearing variables without changing the
library graph. Retaining such a rule while materializing only Dune-selected
artifacts produces dangling support metadata.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name artifact-support))
  > EOF

  $ mkdir support
  $ cat >support/dune <<'EOF'
  > (library
  >  (name artifact_support)
  >  (public_name artifact-support))
  > EOF
  $ echo 'let value = 42' >support/artifact_support.ml

  $ cat >META.artifact-support.template <<'EOF'
  > # DUNE_GEN
  > archive(byte,custom) = "missing.cma"
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package artifact-support))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -predicates byte,custom -format "%d/%A" artifact-support))))
  > EOF

The scoped dependency rejects the override instead of exposing metadata for an
artifact that is absent from the layout.

  $ dune build result
  File "META.artifact-support.template", line 1, characters 0-0:
  Error: Package artifact-support has a META file template that changes library
  dependencies or artifact metadata.
  Such templates cannot be used in a scoped package dependency.
  [1]
