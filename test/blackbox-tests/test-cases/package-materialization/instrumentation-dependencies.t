Instrumentation backend metadata names a PPX library that is not an ordinary
library requirement. The scoped layout must track the referenced library and
its metadata.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name instrumentation-root))
  > (package (name instrumentation-ppx))
  > EOF

  $ mkdir backend ppx
  $ cat >backend/dune <<'EOF'
  > (library
  >  (name backend)
  >  (public_name instrumentation-root)
  >  (modules ())
  >  (instrumentation.backend
  >   (ppx instrumentation-ppx)))
  > EOF

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name ppx)
  >  (public_name instrumentation-ppx)
  >  (kind ppx_rewriter)
  >  (modules ()))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target marker)
  >  (deps (package instrumentation-root))
  >  (action (write-file %{target} "ok")))
  > EOF

The PPX package's metadata is not currently a dependency of the action.

  $ dune rules --format=json _build/default/marker |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep 'lib/instrumentation-.*/dune-package' |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/instrumentation-root/dune-package"
