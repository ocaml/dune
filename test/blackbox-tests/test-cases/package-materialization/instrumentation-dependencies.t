Instrumentation backend metadata names a PPX library that is not an ordinary
library requirement. The scoped layout must track the referenced library and
its metadata.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name instrumentation-root))
  > (package (name instrumentation-old))
  > (package (name instrumentation-middle))
  > (package (name instrumentation-ppx))
  > EOF

  $ mkdir backend old middle ppx consumer
  $ cat >backend/dune <<'EOF'
  > (library
  >  (name direct)
  >  (public_name instrumentation-root.direct)
  >  (modules ())
  >  (instrumentation.backend
  >   (ppx instrumentation-ppx)))
  > (library
  >  (name redirected)
  >  (public_name instrumentation-root.redirected)
  >  (modules ())
  >  (instrumentation.backend
  >   (ppx instrumentation-old.backend)))
  > EOF

  $ cat >old/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name instrumentation-old.backend)
  >  (new_public_name instrumentation-middle.backend))
  > EOF

  $ cat >middle/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name instrumentation-middle.backend)
  >  (new_public_name instrumentation-ppx))
  > EOF

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name ppx)
  >  (public_name instrumentation-ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/ppx.ml <<'EOF'
  > let () = Ppxlib.Driver.register_transformation "instrumentation-noop"
  > EOF

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (executable
  >  (name main)
  >  (instrumentation (backend instrumentation-root.redirected)))
  > EOF
  $ echo 'let () = ()' >consumer/main.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target marker)
  >  (deps (package instrumentation-root))
  >  (action (write-file %{target} "ok")))
  > (rule
  >  (target nested-result)
  >  (deps
  >   (package instrumentation-root)
  >   (source_tree consumer))
  >  (action
  >   (progn
  >    (chdir consumer
  >     (run %{bin:dune} build --instrument-with instrumentation-root.redirected main.exe))
  >    (write-file %{target} "ok"))))
  > EOF

The PPX package's metadata is not currently a dependency of the action.

  $ dune rules --format=json _build/default/marker |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep 'lib/instrumentation-.*/dune-package' |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/instrumentation-root/dune-package"

The nested Dune consumer cannot follow the separately owned redirect chain
named by the serialized instrumentation metadata.

  $ dune build nested-result >err 2>&1; status=$?; test ! -s err || censor <err; (exit $status)
  File "$PWD/_build/install/default/.packages/$DIGEST/lib/instrumentation-root/dune-package", line 64, characters 26-53:
  64 |  (instrumentation.backend instrumentation-old.backend))
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "instrumentation-old.backend" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]
