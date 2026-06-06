Test that package deps and file deps work together in the same (deps ...).

  $ make_dune_project_with_package 3.24 foo

  $ mkdir src

  $ cat >src/dune <<EOF
  > (library (public_name foo))
  > EOF

  $ cat >src/mylib.ml <<EOF
  > let x = 1
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package foo) (file src/mylib.ml))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

  $ dune build out

The layout deps include foo, and the file dep is also present:

  $ dune rules --format=json _build/default/out | jq_dune '.[] | ruleDepFilePaths' | censor | grep -E 'dune-package|src/mylib' | sort
  "_build/default/src/mylib.ml"
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"
