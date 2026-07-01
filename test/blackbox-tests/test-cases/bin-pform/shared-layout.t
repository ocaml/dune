Two rules with the same set of %{bin:...} deps share a single
.binaries directory (keyed by the digest of the sorted unique bin
names).

  $ make_mypkg_bin_project

  $ cat >dune <<'EOF'
  > (rule
  >  (deps %{bin:mybin})
  >  (action
  >   (with-stdout-to out1
  >    (bash "echo $PATH"))))
  > (rule
  >  (deps %{bin:mybin})
  >  (action
  >   (with-stdout-to out2
  >    (bash "echo $PATH"))))
  > EOF

  $ dune build out1 out2

Both rules show the same .binaries digest in PATH (no $DIGEST1 /
$DIGEST2):

  $ env_added "$(cat _build/default/out1)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $ env_added "$(cat _build/default/out2)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
