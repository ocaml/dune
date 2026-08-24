A scoped package layout retains a library's Melange-only requirements in its
`dune-package` file, so it must materialize the corresponding Melange closure.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (using melange 0.1)
  > (package (name melange-root))
  > (package (name melange-support))
  > EOF

  $ mkdir root support consumer
  $ cat >support/dune <<'EOF'
  > (library
  >  (name melange_support)
  >  (public_name melange-support)
  >  (modes melange))
  > EOF
  $ echo 'let value = 42' >support/melange_support.ml

  $ cat >root/dune <<'EOF'
  > (library
  >  (name melange_root)
  >  (public_name melange-root)
  >  (modes melange)
  >  (melange.libraries melange-support))
  > EOF
  $ echo 'let value = Melange_support.value' >root/melange_root.ml

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (modules main)
  >  (libraries melange-root))
  > EOF
  $ echo 'let () = Js.log Melange_root.value' >consumer/main.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps
  >   (package melange-root)
  >   (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} build @melange)))))
  > EOF

The scoped layout follows the serialized `melange_requires` edge, so the nested
Melange build can resolve and compile the support library.

  $ dune build result
