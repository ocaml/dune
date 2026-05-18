Two rules with the same set of %{bin:...} deps share a single
bin-layout directory (keyed by the digest of the sorted unique bin
names).

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

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

Both rules show the same bin-layout digest in PATH (no $DIGEST1 /
$DIGEST2):

  $ env_added "$(cat _build/default/out1)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $PWD/_build/install/default/bin
  $ env_added "$(cat _build/default/out2)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
  $PWD/_build/install/default/bin
