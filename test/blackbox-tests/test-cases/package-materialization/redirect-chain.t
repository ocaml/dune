A scoped package dependency materializes metadata for every intermediate name
in a deprecated-library redirect chain.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name redirect-root))
  > (package (name redirect-middle))
  > (package (name redirect-target))
  > EOF

  $ mkdir root middle target middle-unrelated target-unrelated
  $ cat >root/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name redirect-root.old)
  >  (new_public_name redirect-middle.old))
  > EOF

  $ cat >middle/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name redirect-middle.old)
  >  (new_public_name redirect-target))
  > EOF

  $ cat >target/dune <<'EOF'
  > (library
  >  (name redirect_target)
  >  (public_name redirect-target))
  > EOF
  $ echo 'let value = 42' >target/redirect_target.ml

  $ cat >middle-unrelated/dune <<'EOF'
  > (library
  >  (name middle_unrelated)
  >  (public_name redirect-middle.unrelated))
  > EOF
  $ echo 'let value = ()' >middle-unrelated/unrelated.ml

  $ cat >target-unrelated/dune <<'EOF'
  > (library
  >  (name target_unrelated)
  >  (public_name redirect-target.unrelated))
  > EOF
  $ echo 'let value = ()' >target-unrelated/unrelated.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target recursive)
  >  (deps (package redirect-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  > (rule
  >  (target no-middle-unrelated)
  >  (deps (package redirect-root))
  >  (action
  >   (progn
  >    (bash "! %{bin:ocamlfind} query redirect-middle.unrelated >/dev/null 2>&1")
  >    (write-file %{target} ""))))
  > (rule
  >  (target no-target-unrelated)
  >  (deps (package redirect-root))
  >  (action
  >   (progn
  >    (bash "! %{bin:ocamlfind} query redirect-target.unrelated >/dev/null 2>&1")
  >    (write-file %{target} ""))))
  > EOF

  $ dune build recursive
  $ sort _build/default/recursive | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/redirect-middle/old
  $PWD/_build/install/default/.packages/$DIGEST/lib/redirect-root/old
  $PWD/_build/install/default/.packages/$DIGEST/lib/redirect-target

Unrelated siblings remain absent from the scoped layout.

  $ dune build no-middle-unrelated no-target-unrelated
