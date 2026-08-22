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

The installed META currently retains the conditional archive override even
though that artifact is not part of the package layout.

  $ dune build result
  $ cat _build/default/result | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/artifact-support/missing.cma
  $ test -e "$(cat _build/default/result)"
  [1]
