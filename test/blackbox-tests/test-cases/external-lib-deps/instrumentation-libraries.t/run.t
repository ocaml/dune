`dune describe external-lib-deps --instrument-with` includes libraries that
are active only for the selected instrumentation backend.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name instr_ppx)
  >  (kind ppx_rewriter)
  >  (modules instr_ppx)
  >  (libraries ppxlib))
  > 
  > (library
  >  (name instr)
  >  (modules instr)
  >  (instrumentation.backend
  >   (ppx instr_ppx)))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (instrumentation
  >   (backend instr)
  >   (libraries active.instrumentation.lib)))
  > EOF

  $ cat >instr_ppx.ml <<'EOF'
  > let () = Ppxlib.Driver.register_transformation "instr"
  > EOF
  $ cat >instr.ml <<'EOF'
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = ()
  > EOF

Without instrumentation, the conditional library is ignored.

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (instr_ppx))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ((ppxlib required)))
      (internal_deps ())))))

With instrumentation enabled, the conditional library is reported.

  $ dune describe external-lib-deps --instrument-with instr
  (default
   ((library
     ((names (instr_ppx))
      (extensions ())
      (package ())
      (source_dir .)
      (external_deps ((ppxlib required)))
      (internal_deps ())))
    (executables
     ((names (main))
      (extensions (.exe))
      (package ())
      (source_dir .)
      (external_deps ((active.instrumentation.lib required)))
      (internal_deps ((instr_ppx required)))))))
